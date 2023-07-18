use crate::{
    common::{
        bytecode::{
            disassemble_chunk, disassemble_instruction, Chunk, ExprOpCode, Function, GenericOpCode,
            OpCode, PatternOpCode, Program,
        },
        expect_option, internal_error,
        value::{FunctionKind, Type, Value},
        EridaniFunction,
    },
    runtime::{ArgsFormatter, Error, Result},
};

use core::{mem, ptr::NonNull};

use alloc::collections::VecDeque;

struct Debugger {
    buf: String,
    dropped_in_panic: bool,
}

#[derive(Debug)]
struct CallFrame {
    code: NonNull<Chunk>,
    concatenation_match_start: Option<usize>,
    ip: usize,
    frame_start: usize,
    variables: Vec<Value>,
    list_starts: Vec<usize>,
}

impl CallFrame {
    fn new(chunk: &Chunk, frame_start: usize, variables: Vec<Value>) -> CallFrame {
        CallFrame {
            code: NonNull::from(chunk),
            concatenation_match_start: None,
            ip: 0,
            frame_start,
            variables,
            list_starts: Vec::with_capacity(8),
        }
    }
}

pub(super) struct Vm {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    functions: Vec<Function>,
    natives: Vec<(Box<dyn EridaniFunction>, String)>,
    entry_point: u16,
    debugger: Debugger,
}

impl Vm {
    pub fn new(program: Program) -> Self {
        let Program {
            functions,
            natives,
            entry_point,
        } = program;
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(32),
            functions,
            natives,
            entry_point,
            debugger: Debugger { buf: String::with_capacity(1024), dropped_in_panic: true }
        }
    }

    fn error(&self, kind: String, message: String) -> Error {
        Error::new(
            kind,
            message,
            self.current_chunk().lines()[self.current_callframe().ip],
        )
    }

    fn current_callframe(&self) -> &CallFrame {
        expect_option!(self.frames.last(), "no callframe")
    }

    fn current_callframe_mut(&mut self) -> &mut CallFrame {
        expect_option!(self.frames.last_mut(), "no callframe")
    }

    fn current_list_start(&mut self) -> usize {
        expect_option!(
            self.current_callframe_mut().list_starts.pop(),
            "no list start"
        )
    }

    fn current_chunk(&self) -> &Chunk {
        // SAFETY: `frame.code` is derived from `&Chunk`,
        // and `Vm.functions` is never mutated
        unsafe {
            expect_option!(self.frames.last(), "no callframe")
                .code
                .as_ref()
        }
    }

    fn pop_stack(&mut self) -> Value {
        expect_option!(self.stack.pop(), "popped empty stack")
    }

    fn stack_top(&self) -> &Value {
        expect_option!(self.stack.last(), "read top of empty stack")
    }

    fn concatenation_match_start(&self) -> usize {
        expect_option!(self.current_callframe().concatenation_match_start, "read 'concatenation_match_start' outside of concatenation")
    }

    fn pop_frame(&mut self) -> CallFrame {
        expect_option!(self.frames.pop(), "popped empty frames")
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.current_callframe_mut();
        // SAFETY: `frame.code` is derived from `&Chunk`,
        // and `Vm.functions` is never mutated
        let byte = unsafe { frame.code.as_ref() }.code()[frame.ip];
        frame.ip += 1;
        byte
    }

    fn read_opcode(&mut self) -> OpCode {
        let frame = self.current_callframe_mut();
        // SAFETY: `frame.code` is derived from `&Chunk`,
        // and `Vm.functions` is never mutated
        let chunk = unsafe { frame.code.as_ref() };
        let byte = chunk.code()[frame.ip];
        #[cfg(debug_assertions)]
        disassemble_instruction(chunk, frame.ip);
        frame.ip += 1;
        OpCode::from(byte)
    }

    fn read_bytes(&mut self) -> u16 {
        let frame = self.current_callframe_mut();
        // SAFETY: `frame.code` is derived from `&Chunk`,
        // and `Vm.functions` is never mutated
        let chunk = unsafe { frame.code.as_ref() };
        let byte1 = chunk.code()[frame.ip];
        let byte2 = chunk.code()[frame.ip + 1];
        frame.ip += 2;
        u16::from_be_bytes([byte1, byte2])
    }

    fn generic_op_code(&mut self, op_code: GenericOpCode) -> Result<()> {
        match op_code {
            GenericOpCode::Constant => {
                let byte = self.read_byte();
                self.stack
                    .push(self.current_chunk().constants()[usize::from(byte)].to_owned());
            }
            GenericOpCode::WideConstant => {
                let bytes = self.read_bytes();
                self.stack
                    .push(self.current_chunk().constants()[usize::from(bytes)].to_owned());
            }
            GenericOpCode::Nothing => self.stack.push(Value::Nothing),
            GenericOpCode::Add => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                let result = (&value2 + &value1).ok_or(self.error(
                    "Arithmetic".to_string(),
                    format!("Cannot add {value2} to {value1}"),
                ))?;
                self.stack.push(result);
            }
            GenericOpCode::Subtract => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                let result = (&value2 - &value1).ok_or(self.error(
                    "Arithmetic".to_string(),
                    format!("Cannot subtract {value2} from {value1}"),
                ))?;
                self.stack.push(result);
            }
            GenericOpCode::Multiply => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                let result = (&value2 * &value1).ok_or(self.error(
                    "Arithmetic".to_string(),
                    format!("Cannot multiply {value2} by {value1}"),
                ))?;
                self.stack.push(result);
            }
            GenericOpCode::Divide => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                let result = (&value2 / &value1).ok_or(self.error(
                    "Arithmetic".to_string(),
                    format!("Cannot add {value2} to {value1}"),
                ))?;
                self.stack.push(result);
            }
            GenericOpCode::Modulo => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                let result = (&value2 % &value1).ok_or(self.error(
                    "Arithmetic".to_string(),
                    format!("Cannot add {value2} to {value1}"),
                ))?;
                self.stack.push(result);
            }
            GenericOpCode::Negate => {
                let value = self.pop_stack();
                let result = (-&value).ok_or(
                    self.error("Arithmetic".to_string(), format!("Cannot negate {value}")),
                )?;
                self.stack.push(result);
            }
            GenericOpCode::GetVar => {
                let byte = self.read_byte();
                self.stack
                    .push(self.current_callframe().variables[usize::from(byte)].to_owned());
            }
            GenericOpCode::GetVarWide => {
                let bytes = self.read_bytes();
                self.stack
                    .push(self.current_callframe().variables[usize::from(bytes)].to_owned());
            }
            GenericOpCode::Pop => {
                self.pop_stack();
            }
        }

        Ok(())
    }

    fn expr_op_code(&mut self, op_code: ExprOpCode) -> Result<bool> {
        match op_code {
            ExprOpCode::StartList => {
                let list_start = self.stack.len();
                self.current_callframe_mut().list_starts.push(list_start);
            }
            ExprOpCode::Call => {
                let args_start = self.current_list_start();
                let callee = self.pop_stack();
                match callee {
                    Value::Function(function) => {
                        if function.kind == FunctionKind::Eridani {
                            self.function(function.reference, args_start)?;
                            #[cfg(debug_assertions)]
                            println!();
                        } else {
                            let (native, name) = &mut self.natives[usize::from(function.reference)];
                            let value = native(&self.stack[args_start..]).map_err(|error| {
                                Error::from_argument_error(error, name.to_owned())
                            })?;
                            self.stack.truncate(args_start);
                            self.stack.push(value);
                        }
                    }
                    Value::Method(method) => {
                        #[cfg(debug_assertions)]
                        disassemble_chunk(
                            &method.parameters().0,
                            "<lambda expression>",
                            "parameters",
                            0,
                        );

                        self.frames.push(CallFrame::new(
                            &method.parameters().0,
                            args_start,
                            vec![],
                        ));
                        let num_args = self.stack.len() - args_start;
                        if num_args == method.num_parameters()
                            && self.pattern()?
                        {
                            let frame = self.pop_frame();
                            let variables = frame.variables;
                            self.stack.truncate(args_start);

                            #[cfg(debug_assertions)]
                            disassemble_chunk(method.chunk(), "<lambda expression>", "body", 0);

                            self.frames.push(CallFrame::new(
                                method.chunk(),
                                self.stack.len(),
                                variables,
                            ));
                            self.expr()?;
                            self.pop_frame();
                        } else {
                            self.pop_frame();
                            return Err(self.error(
                                "Match".to_string(),
                                format!(
                                    "Lambda expression does not match the arguments '{}'",
                                    ArgsFormatter(
                                        &self.stack
                                            [args_start..args_start + num_args]
                                    )
                                ),
                            ));
                        }
                    }
                    _ => {
                        return Err(
                            self.error("Type".to_string(), format!("Cannot call '{callee}'"))
                        )
                    }
                }
            }
            ExprOpCode::List => {
                let list_start = self.current_list_start();
                let mut current_element = list_start;
                let mut list = VecDeque::with_capacity(self.stack.len() - current_element);
                while current_element < self.stack.len() {
                    list.push_back(mem::replace(
                        &mut self.stack[current_element],
                        Value::Nothing,
                    ));
                    current_element += 1;
                }
                self.stack.truncate(list_start);
                self.stack.push(Value::List(list));
            }
            ExprOpCode::Spread => {
                let value = self.pop_stack();
                if let Value::List(mut list) = value {
                    let (start, end) = list.as_mut_slices();
                    for element in start {
                        self.stack.push(mem::replace(element, Value::Nothing));
                    }
                    for element in end {
                        self.stack.push(mem::replace(element, Value::Nothing));
                    }
                } else {
                    return Err(self.error(
                        "Type".to_string(),
                        "Cannot use '..' on a non-list".to_string(),
                    ));
                }
            }
            ExprOpCode::Pattern => {
                self.pattern()?;
            }
            ExprOpCode::Return => return Ok(true),
        }

        Ok(false)
    }

    fn pattern_op_code(&mut self, op_code: PatternOpCode) -> Option<bool> {
        match op_code {
            PatternOpCode::HoistValue => {
                let byte = self.read_byte();
                self.stack.push(
                    self.stack[self.current_callframe().frame_start + usize::from(byte)].to_owned(),
                );
            }
            PatternOpCode::DuplicateValue => {
                let value = expect_option!(self.stack.last(), "read empty stack");
                self.stack.push(value.to_owned());
            }
            PatternOpCode::Equal => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                if value2 == value1 {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
            }
            PatternOpCode::NotEqual => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                if value2 != value1 {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
            }
            PatternOpCode::Greater => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                if value2 > value1 {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
            }
            PatternOpCode::GreaterEqual => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                if value2 >= value1 {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
            }
            PatternOpCode::Less => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                if value2 < value1 {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
            }
            PatternOpCode::LessEqual => {
                let value1 = self.pop_stack();
                let value2 = self.pop_stack();
                if value2 <= value1 {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
            }
            PatternOpCode::Type => {
                let kind = Type::from(self.read_byte());
                let value = self.pop_stack();
                if kind.is_kind(&value) {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
            }
            PatternOpCode::SplitList => {
                let value = self.pop_stack();
                if let Value::List(mut list) = value {
                    let head = if let Some(head) = list.pop_front() {
                        head
                    } else {
                        return Some(false);
                    };
                    self.stack.push(Value::List(list));
                    self.stack.push(head);
                } else {
                    return Some(false);
                }
            }
            PatternOpCode::PushVar => {
                let value = self.pop_stack();
                self.current_callframe_mut().variables.push(value);
            }
            PatternOpCode::StartConcat => {
                self.current_callframe_mut().concatenation_match_start = Some(0);
            }
            PatternOpCode::ConcatCompare => {
                let value = self.pop_stack().into_string();
                let base = self.stack_top().expect_string();
                let match_start = self.concatenation_match_start();
                if base[match_start..].starts_with(&value) {
                    self.current_callframe_mut().concatenation_match_start = Some(match_start + value.len());
                } else {
                    return Some(false);
                }
            }
            PatternOpCode::ConcatCompareVarWaiting => {
                let value = self.pop_stack().into_string();
                let base = self.stack_top().expect_string();
                let match_start = self.concatenation_match_start();
                if let Some(value_start) = base.find(&value) {
                    let var = Value::String(base[match_start..value_start].to_string());
                    let new_match_start = value_start + value.len();
                    let frame = self.current_callframe_mut();
                    frame.variables.push(var);
                    frame.concatenation_match_start = Some(new_match_start);
                } else {
                    return Some(false);
                }
            }
            PatternOpCode::EndConcat => {
                let value = self.pop_stack().into_string();
                if value.is_empty() {
                    self.stack.push(Value::Number(1.0));
                } else {
                    self.stack.push(Value::Number(0.0));
                }
                self.current_callframe_mut().concatenation_match_start = None;
            }
            PatternOpCode::EndConcatWithVar => {
                let value = self.pop_stack().into_string();
                let match_start = self.concatenation_match_start();
                self.current_callframe_mut().variables.push(Value::String(value[match_start..].to_string()));
                self.stack.push(Value::Number(1.0));
                self.current_callframe_mut().concatenation_match_start = None;
            }
            PatternOpCode::JumpIfTrue => {
                let bytes = self.read_bytes();
                let value = self.pop_stack();
                if value == Value::Number(1.0) {
                    self.current_callframe_mut().ip = bytes.into();
                }
            }
            PatternOpCode::BreakIfTrue => {
                let value = self.pop_stack();
                if value == Value::Number(1.0) {
                    return Some(false);
                }
            }
            PatternOpCode::BreakIfFalse => {
                let value = self.pop_stack();
                if value == Value::Number(0.0) {
                    return Some(false);
                }
            }
            PatternOpCode::EndPattern => {
                let value = self.pop_stack();
                if value == Value::Number(1.0) {
                    return Some(true);
                } else {
                    return Some(false);
                }
            }
            PatternOpCode::PatternSuccess => return Some(true),
        }

        None
    }

    fn expr(&mut self) -> Result<()> {
        loop {
            match self.read_opcode() {
                OpCode::Generic(op_code) => self.generic_op_code(op_code)?,
                OpCode::Expr(op_code) => {
                    if self.expr_op_code(op_code)? {
                        return Ok(());
                    }
                }
                OpCode::Pattern(_) => internal_error!("encountered pattern opcode in expr"),
            }
        }
    }

    fn pattern(&mut self) -> Result<bool> {
        loop {
            match self.read_opcode() {
                OpCode::Generic(op_code) => self.generic_op_code(op_code)?,
                OpCode::Pattern(op_code) => {
                    if let Some(result) = self.pattern_op_code(op_code) {
                        return Ok(result);
                    }
                }
                OpCode::Expr(_) => internal_error!("encountered expr opcode in pattern"),
            }
        }
    }

    fn function(&mut self, index: u16, args_start: usize) -> Result<()> {
        let mut method_index = None;
        let mut i = 0;
        let num_methods = self.functions[index as usize].methods().len();
        let num_args = self.stack.len() - args_start;
        while i < num_methods {
            if i > 0 {
                self.pop_frame();
            }
            let method = &self.functions[index as usize].methods()[i];
            if method.num_parameters() != num_args {
                i += 1;
                continue;
            }
            #[cfg(debug_assertions)]
            disassemble_chunk(
                &method.parameters().0,
                self.functions[index as usize].name(),
                "parameters",
                i,
            );
            self.frames
                .push(CallFrame::new(&method.parameters().0, args_start, vec![]));
            if self.pattern()? {
                method_index = Some(i);
                break;
            }
            i += 1;
        }

        let method_index = if let Some(method_index) = method_index {
            method_index
        } else {
            return Err(self.error("Match".to_string(), format!("Function '{}' has no methods that match the arguments '{}'", self.functions[index as usize].name(), ArgsFormatter(&self.stack[args_start..args_start + num_args]))));
        };
        let frame = self.pop_frame();
        let variables = frame.variables;
        self.stack.truncate(frame.frame_start);

        let chunk = self.functions[index as usize].methods()[method_index].chunk();

        #[cfg(debug_assertions)]
        disassemble_chunk(
            chunk,
            self.functions[index as usize].name(),
            "body",
            method_index,
        );

        self.frames.push(CallFrame::new(
            self.functions[index as usize].methods()[method_index].chunk(),
            self.stack.len(),
            variables,
        ));

        self.expr()?;

        self.pop_frame();

        Ok(())
    }

    pub fn run(&mut self, mut args: Vec<Value>) -> Result<Value> {
        self.stack.append(&mut args);
        self.function(self.entry_point, 0)?;
        Ok(expect_option!(self.stack.pop(), "missing return value"))
    }
}
