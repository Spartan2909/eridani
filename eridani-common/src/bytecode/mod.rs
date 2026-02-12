#[cfg(feature = "std")]
pub mod disassembler;
#[cfg(feature = "serde")]
mod serialise;

use crate::{discriminant::TargetFeatures, internal_error, natives::NativeFunction, value::Value};

use core::mem;

use alloc::{string::String, vec, vec::Vec};

use hashbrown::HashMap;

use strum::EnumCount;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum GenericOpCode {
    Constant,
    WideConstant,
    Nothing,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Negate,
    GetVar,
    GetVarWide,
    Pop,
}

impl From<u8> for GenericOpCode {
    fn from(value: u8) -> Self {
        if value < GenericOpCode::COUNT as u8 {
            // SAFETY: guaranteed by condition
            unsafe { mem::transmute::<u8, GenericOpCode>(value) }
        } else {
            internal_error!("cannot interpret '0b{:08b}' as generic opcode", value);
        }
    }
}

impl From<GenericOpCode> for u8 {
    fn from(value: GenericOpCode) -> Self {
        value as u8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum ExprOpCode {
    StartList = EXPR_OP_CODE_START,
    Call,
    List,
    Spread,
    Pattern,
    Return,
}

impl From<u8> for ExprOpCode {
    fn from(value: u8) -> Self {
        if (EXPR_OP_CODE_START..PATTERN_OP_CODE_START).contains(&value) {
            // SAFETY: guaranteed by condition
            unsafe { mem::transmute::<u8, ExprOpCode>(value) }
        } else {
            internal_error!("cannot interpret '0b{:08b}' as expr opcode", value);
        }
    }
}

impl From<ExprOpCode> for u8 {
    fn from(value: ExprOpCode) -> Self {
        value as u8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum PatternOpCode {
    HoistValue = PATTERN_OP_CODE_START,
    DuplicateValue,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Type,
    SplitList,
    PushVar,
    StartConcat,
    ConcatCompare,
    ConcatCompareVarWaiting,
    EndConcat,
    EndConcatWithVar,
    JumpIfTrue,
    BreakIfTrue,
    BreakIfFalse,
    EndPattern,
    PatternSuccess,
}

impl From<u8> for PatternOpCode {
    fn from(value: u8) -> Self {
        if value >= PATTERN_OP_CODE_START
            && (value as usize) < PATTERN_OP_CODE_START as usize + PatternOpCode::COUNT
        {
            // SAFETY: guaranteed by condition
            unsafe { mem::transmute::<u8, PatternOpCode>(value) }
        } else {
            internal_error!("cannot interpret '0b{:08b}' as pattern opcode", value);
        }
    }
}

impl From<PatternOpCode> for u8 {
    fn from(value: PatternOpCode) -> Self {
        value as u8
    }
}

const EXPR_OP_CODE_START: u8 = GenericOpCode::COUNT as u8;

const PATTERN_OP_CODE_START: u8 = GenericOpCode::COUNT as u8 + ExprOpCode::COUNT as u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum OpCode {
    Generic(GenericOpCode),
    Expr(ExprOpCode),
    Pattern(PatternOpCode),
}

impl GenericOpCode {
    #[must_use]
    pub fn to_wide(self) -> Self {
        match self {
            GenericOpCode::Constant => GenericOpCode::WideConstant,
            GenericOpCode::GetVar => GenericOpCode::GetVarWide,
            _ => internal_error!("called 'into_wide' on {:?}", self),
        }
    }
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        if (value as usize) < GenericOpCode::COUNT {
            OpCode::Generic(value.into())
        } else if value < PATTERN_OP_CODE_START {
            OpCode::Expr(value.into())
        } else if value < PATTERN_OP_CODE_START + PatternOpCode::COUNT as u8 {
            OpCode::Pattern(value.into())
        } else {
            internal_error!("cannot interpret '0b{:08b}' as opcode", value);
        }
    }
}

impl From<OpCode> for u8 {
    fn from(value: OpCode) -> Self {
        match value {
            OpCode::Generic(op_code) => op_code as u8,
            OpCode::Expr(op_code) => op_code as u8,
            OpCode::Pattern(op_code) => op_code as u8,
        }
    }
}

fn to_bytes(value: u16) -> (Option<u8>, u8) {
    u8::try_from(value).map_or_else(
        |_| {
            let left = ((value & 0xff00) >> 8) as u8;
            let right = value as u8;
            (Some(left), right)
        },
        |value| (None, value),
    )
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    #[must_use]
    pub const fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn push_instruction<T: Into<u8>>(&mut self, op_code: T, line: usize) {
        self.code.push(op_code.into());
        self.lines.push(line);
    }

    #[must_use]
    pub fn push_jump<T: Into<u8>>(&mut self, op_code: T, line: usize) -> usize {
        self.code.push(op_code.into());
        self.code.push(0);
        self.code.push(0);
        self.lines.push(line);
        self.lines.push(line);
        self.lines.push(line);
        self.code.len() - 2
    }

    pub fn push_wide_instruction<T: Into<u8>>(&mut self, op_code: T, byte: u8, line: usize) {
        self.code.push(op_code.into());
        self.code.push(byte);
        self.lines.push(line);
        self.lines.push(line);
    }

    pub fn push_variable_width_instruction(
        &mut self,
        op_code: GenericOpCode,
        bytes: u16,
        line: usize,
    ) {
        let (left, right) = to_bytes(bytes);
        if let Some(left) = left {
            self.code.push(op_code.to_wide().into());
            self.code.push(left);
            self.code.push(right);
            self.lines.push(line);
        } else {
            self.code.push(op_code.into());
            self.code.push(right);
        }
        self.lines.push(line);
        self.lines.push(line);
    }

    pub fn push_constant_instruction(&mut self, constant: Value, line: usize) {
        self.constants.push(constant);
        let (left, right) = to_bytes((self.constants.len() - 1) as u16);
        if let Some(left) = left {
            self.code.push(GenericOpCode::WideConstant.into());
            self.code.push(left);
            self.code.push(right);
            self.lines.push(line);
        } else {
            self.code.push(GenericOpCode::Constant.into());
            self.code.push(right);
        }
        self.lines.push(line);
        self.lines.push(line);
    }

    #[must_use]
    pub fn code(&self) -> &[u8] {
        &self.code
    }

    #[must_use]
    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    #[must_use]
    pub fn lines(&self) -> &[usize] {
        &self.lines
    }

    pub fn patch_jumps(&mut self, jumps: Vec<usize>, destination: usize) {
        let (dest_left, dest_right) = to_bytes(destination as u16);
        let dest_left = dest_left.unwrap_or(0);
        for jump in jumps {
            self.code[jump] = dest_left;
            self.code[jump + 1] = dest_right;
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Parameters(pub Chunk);

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Method {
    chunk: Chunk,
    parameters: Parameters,
    num_parameters: usize,
}

impl Method {
    #[must_use]
    pub const fn new(chunk: Chunk, parameters: Parameters, num_parameters: usize) -> Method {
        Method {
            chunk,
            parameters,
            num_parameters,
        }
    }

    #[must_use]
    pub const fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    #[must_use]
    pub const fn parameters(&self) -> &Parameters {
        &self.parameters
    }

    #[must_use]
    pub const fn num_parameters(&self) -> usize {
        self.num_parameters
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Function {
    methods: Vec<Method>,
    #[cfg_attr(feature = "serde", serde(skip))]
    memo: HashMap<Vec<Value>, Value>,
    name: String,
}

impl Function {
    #[must_use]
    pub fn new(methods: Vec<Method>, name: String) -> Function {
        Function {
            methods,
            memo: HashMap::new(),
            name,
        }
    }

    #[must_use]
    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    pub const fn memo(&mut self) -> &mut HashMap<Vec<Value>, Value> {
        &mut self.memo
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(into = "serialise::SerialiseProgram"))]
#[cfg_attr(feature = "serde", serde(try_from = "serialise::SerialiseProgram"))]
pub struct Program {
    pub functions: Vec<Function>,
    pub natives: Vec<NativeFunction>,
    pub entry_point: u16,
    pub features: TargetFeatures,
}
