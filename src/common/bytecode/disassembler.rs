use crate::common::{
    bytecode::{Chunk, ExprOpCode, GenericOpCode, OpCode, PatternOpCode},
    value::Type,
};

pub(crate) fn disassemble_chunk(
    chunk: &Chunk,
    function_name: &str,
    chunk_kind: &str,
    method_index: usize,
) {
    println!(
        "\n== '{function_name}' {chunk_kind} {method_index} == ({} bytes)",
        chunk.code.len()
    );

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
    println!("== end {chunk_kind} ==\n");
}

pub(crate) fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:01$} ", offset, 4);

    if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
        print!("   | ");
    } else {
        print!("{:>4} ", chunk.lines[offset]);
    }

    match OpCode::from(chunk.code[offset]) {
        OpCode::Generic(op_code) => match op_code {
            GenericOpCode::Constant => constant_instruction("Constant", chunk, offset),
            GenericOpCode::WideConstant => wide_constant_instruction("WideConstant", chunk, offset),
            GenericOpCode::Nothing => simple_instruction("Nothing", offset),
            GenericOpCode::Add => simple_instruction("Add", offset),
            GenericOpCode::Subtract => simple_instruction("Subtract", offset),
            GenericOpCode::Multiply => simple_instruction("Multiply", offset),
            GenericOpCode::Divide => simple_instruction("Divide", offset),
            GenericOpCode::Modulo => simple_instruction("Modulo", offset),
            GenericOpCode::Negate => simple_instruction("Negate", offset),
            GenericOpCode::GetVar => byte_instruction("GetVar", chunk, offset),
            GenericOpCode::GetVarWide => wide_byte_instruction("GetVarWide", chunk, offset),
            GenericOpCode::Pop => simple_instruction("Pop", offset),
        },
        OpCode::Expr(op_code) => match op_code {
            ExprOpCode::StartList => simple_instruction("StartList", offset),
            ExprOpCode::Call => simple_instruction("Call", offset),
            ExprOpCode::List => simple_instruction("List", offset),
            ExprOpCode::Spread => simple_instruction("Spread", offset),
            ExprOpCode::Pattern => simple_instruction("Pattern", offset),
            ExprOpCode::Return => simple_instruction("Return", offset),
        },
        OpCode::Pattern(op_code) => match op_code {
            PatternOpCode::HoistValue => byte_instruction("HoistValue", chunk, offset),
            PatternOpCode::DuplicateValue => simple_instruction("DuplicateValue", offset),
            PatternOpCode::Equal => simple_instruction("Equal", offset),
            PatternOpCode::NotEqual => simple_instruction("NotEqual", offset),
            PatternOpCode::Greater => simple_instruction("Greater", offset),
            PatternOpCode::GreaterEqual => simple_instruction("GreaterEqual", offset),
            PatternOpCode::Less => simple_instruction("Less", offset),
            PatternOpCode::LessEqual => simple_instruction("LessEqual", offset),
            PatternOpCode::Type => type_instruction("Type", chunk, offset),
            PatternOpCode::SplitList => simple_instruction("SplitList", offset),
            PatternOpCode::PushVar => simple_instruction("PushVar", offset),
            PatternOpCode::StartConcat => simple_instruction("StartConcat", offset),
            PatternOpCode::ConcatCompare => simple_instruction("ConcatCompare", offset),
            PatternOpCode::ConcatCompareVarWaiting => simple_instruction("ConcatCompareVarWaiting", offset),
            PatternOpCode::EndConcat => simple_instruction("EndConcat", offset),
            PatternOpCode::EndConcatWithVar => simple_instruction("EndConcatWithVar", offset),
            PatternOpCode::JumpIfTrue => wide_byte_instruction("JumpIfTrue", chunk, offset),
            PatternOpCode::BreakIfTrue => simple_instruction("BreakIfTrue", offset),
            PatternOpCode::BreakIfFalse => simple_instruction("BreakIfFalse", offset),
            PatternOpCode::EndPattern => simple_instruction("EndPattern", offset),
            PatternOpCode::PatternSuccess => simple_instruction("PatternSuccess", offset),
        },
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code[offset + 1];
    println!(
        "{:<16} {:>5} '{}'",
        name,
        constant,
        chunk.constants[usize::from(constant)]
    );

    offset + 2
}

fn wide_constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let high = chunk.code[offset + 1];
    let low = chunk.code[offset + 2];
    let constant = (u16::from(high) << 8) | u16::from(low);
    println!(
        "{:<16} {:>5} '{}'",
        name,
        constant,
        chunk.constants[usize::from(constant)]
    );

    offset + 3
}

fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    println!("{:<16} {:>5}", name, slot);

    offset + 2
}

fn wide_byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let high = chunk.code[offset + 1];
    let low = chunk.code[offset + 2];
    let slot = (u16::from(high) << 8) | u16::from(low);
    println!("{:<16} {:>5}", name, slot);

    offset + 3
}

fn type_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let kind = chunk.code[offset + 1];
    println!("{:<16} {:>5} '{:?}'", name, kind, Type::from(kind));

    offset + 2
}
