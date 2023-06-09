use crate::{
    common::{internal_error, value::Value},
    compiler::{
        analyser::Environment,
        parser,
        scanner::{Token, TokenType},
    },
    prelude::*,
};

use strum::EnumCount;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Integer,
    List,
    Number,
    String,
}

fn is_kind(this: &Value, kind: Type) -> bool {
    match (this, kind) {
        (Value::Number(num), Type::Integer) => num.fract() == 0.0,
        (Value::List(_), Type::List) => true,
        (Value::Number(_), Type::Number) => true,
        (Value::String(_), Type::String) => true,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparision {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Comparision {
    fn compare(self, lhs: &Value, rhs: &Value) -> bool {
        match self {
            Comparision::Equal => lhs == rhs,
            Comparision::Greater => lhs > rhs,
            Comparision::GreaterEqual => lhs >= rhs,
            Comparision::Less => lhs < rhs,
            Comparision::LessEqual => lhs <= rhs,
            Comparision::NotEqual => lhs != rhs,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Value(Value),
    Wildcard(Variable),
}

impl Item {
    fn resolve<'a>(&'a self, variables: &'a [Value]) -> Option<&'a Value> {
        match self {
            Item::Value(value) => Some(value),
            Item::Wildcard(var) => variables.get(var.reference() as usize),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperatorChain {
    operator: ArithOp,
    mid: Item,
    next: Option<Box<OperatorChain>>,
}

impl OperatorChain {
    fn bind(&mut self, environment: &mut Environment) {
        if let Item::Wildcard(Variable::Name(name)) = &self.mid {
            let reference = environment.get_or_add(name.to_owned());
            self.mid = Item::Wildcard(Variable::Reference(reference));
        }

        if let Some(next) = &mut self.next {
            next.bind(environment);
        }
    }

    fn references(&self, references: &mut Vec<u16>) {
        if let Item::Wildcard(Variable::Reference(reference)) = &self.mid {
            references.push(*reference);
        }

        if let Some(next) = &self.next {
            next.references(references);
        }
    }
}

impl From<&parser::OperatorChain> for OperatorChain {
    fn from(value: &parser::OperatorChain) -> Self {
        let operator = match value.operator().kind() {
            TokenType::Plus => ArithOp::Add,
            TokenType::Minus => ArithOp::Sub,
            TokenType::Star => ArithOp::Mul,
            TokenType::Slash => ArithOp::Div,
            TokenType::Mod => ArithOp::Mod,
            _ => internal_error!("parsed token '{:?}' as operator", value.operator()),
        };

        let mid: Item = value.mid().into();

        let next = value.next().map(|next| Box::new(next.into()));

        OperatorChain {
            operator,
            mid,
            next,
        }
    }
}

impl From<parser::OperatorChain> for OperatorChain {
    fn from(value: parser::OperatorChain) -> Self {
        (&value).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Name(String),
    Reference(u16),
}

impl Variable {
    fn reference(&self) -> u16 {
        if let Variable::Reference(reference) = self {
            *reference
        } else {
            internal_error!("called 'reference' on a named variable");
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Pattern {
    Binary {
        left: Box<Pattern>,
        operator: LogOp,
        right: Box<Pattern>,
    },
    Comparision {
        comparison: Comparision,
        rhs: Item,
    },
    List {
        left: Box<Pattern>,
        right: Box<Pattern>,
    },
    Literal(Value),
    OperatorComparison {
        operator_chain: OperatorChain,
        comparison: Comparision,
        rhs: Item,
    },
    Range {
        lower: Value,
        upper: Value,
        inclusive: bool,
    },
    Type(Type),
    Unary {
        operator: UnOp,
        right: Box<Pattern>,
    },
    Wildcard(Option<Variable>),
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount)]
enum PatternPrecedence {
    Wildcard = 10,
    Type = 20,
    Comparision = 30,
    Range = 40,
    Literal = 50,
}

impl From<PatternPrecedence> for i32 {
    fn from(value: PatternPrecedence) -> Self {
        (value as u8).into()
    }
}

impl Pattern {
    pub fn matches(&self, value: &Value, variables: Vec<Value>) -> Option<Vec<Value>> {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                if *operator == LogOp::Or {
                    let left = left.matches(value, variables.clone());
                    let right = right.matches(value, variables.clone());
                    if left.is_some() || right.is_some() {
                        Some(variables)
                    } else {
                        None
                    }
                } else {
                    let variables = left.matches(value, variables)?;
                    let variables = right.matches(value, variables)?;
                    Some(variables)
                }
            }
            Pattern::Comparision { comparison, rhs } => {
                let rhs = rhs.resolve(&variables)?;
                let matches = comparison.compare(value, rhs);

                if matches {
                    Some(variables)
                } else {
                    None
                }
            }
            Pattern::List { left, right } => {
                if let Value::List(list) = value {
                    let variables = left.matches(value, variables)?;
                    let tail = match list.get(1) {
                        Some(_) => Value::List(list[1..].to_vec()),
                        None => Value::Nothing,
                    };
                    let variables = right.matches(&tail, variables)?;
                    Some(variables)
                } else {
                    None
                }
            }
            Pattern::Literal(literal) => {
                if value == literal {
                    Some(variables)
                } else {
                    None
                }
            }
            Pattern::OperatorComparison {
                operator_chain,
                comparison,
                rhs,
            } => {
                let mut operator_chain = Some(operator_chain);
                let mut value = value.to_owned();

                while let Some(operator) = operator_chain {
                    let mid = operator.mid.resolve(&variables)?.clone();

                    value = match operator.operator {
                        ArithOp::Add => value + mid,
                        ArithOp::Sub => value - mid,
                        ArithOp::Mul => value * mid,
                        ArithOp::Div => value / mid,
                        ArithOp::Mod => value % mid,
                    }?;

                    operator_chain = match &operator.next {
                        Some(next) => Some(next),
                        None => None,
                    }
                }

                let rhs = rhs.resolve(&variables)?;
                let matches = comparison.compare(&value, rhs);

                if matches {
                    Some(variables)
                } else {
                    None
                }
            }
            Pattern::Range {
                lower,
                upper,
                inclusive,
            } => {
                if lower > value {
                    return None;
                }

                let matches = if *inclusive {
                    value <= upper
                } else {
                    value < upper
                };

                if matches {
                    Some(variables)
                } else {
                    None
                }
            }
            Pattern::Type(kind) => {
                if is_kind(value, *kind) {
                    Some(variables)
                } else {
                    None
                }
            }
            Pattern::Unary { operator, right } => match *operator {
                UnOp::Not => {
                    if right.matches(value, variables.clone()).is_some() {
                        None
                    } else {
                        Some(variables)
                    }
                }
            },
            Pattern::Wildcard(label) => {
                let mut variables = variables;
                if let Some(label) = label {
                    if let Some(existing_value) = variables.get(label.reference() as usize) {
                        if value != existing_value {
                            return None;
                        }
                    } else {
                        variables.push(value.clone());
                    }
                }

                Some(variables)
            }
        }
    }

    pub fn bind(&mut self, environment: &mut Environment) {
        match self {
            Pattern::Binary { left, right, .. } => {
                left.bind(environment);
                right.bind(environment);
            }
            Pattern::Comparision { rhs, .. } => {
                if let Item::Wildcard(Variable::Name(name)) = rhs {
                    let reference = environment.get_or_add(name.to_owned());
                    *rhs = Item::Wildcard(Variable::Reference(reference));
                }
            }
            Pattern::List { left, right } => {
                left.bind(environment);
                right.bind(environment);
            }
            Pattern::Literal(_) => {}
            Pattern::OperatorComparison {
                operator_chain,
                rhs,
                ..
            } => {
                operator_chain.bind(environment);

                if let Item::Wildcard(Variable::Name(name)) = rhs {
                    let reference = environment.get_or_add(name.to_owned());
                    *rhs = Item::Wildcard(Variable::Reference(reference));
                }
            }
            Pattern::Range { .. } => {}
            Pattern::Type(_) => {}
            Pattern::Unary { right, .. } => right.bind(environment),
            Pattern::Wildcard(binding) => {
                if let Some(Variable::Name(name)) = binding {
                    let reference = environment.get_or_add(name.to_owned());
                    *self = Pattern::Wildcard(Some(Variable::Reference(reference)));
                }
            }
        }
    }

    pub fn references(&self) -> Vec<u16> {
        match self {
            Pattern::Binary { left, right, .. } => {
                let mut references = left.references();
                references.append(&mut right.references());
                references
            }
            Pattern::Comparision { rhs, .. } => {
                if let Item::Wildcard(var) = rhs {
                    vec![var.reference()]
                } else {
                    vec![]
                }
            }
            Pattern::List { left, right } => {
                let mut references = left.references();
                references.append(&mut right.references());
                references
            }
            Pattern::OperatorComparison {
                operator_chain,
                rhs,
                ..
            } => {
                let mut references = vec![];
                operator_chain.references(&mut references);
                if let Item::Wildcard(var) = rhs {
                    references.push(var.reference());
                }
                references
            }
            Pattern::Unary { right, .. } => right.references(),
            Pattern::Wildcard(var) => {
                if let Some(var) = var {
                    vec![var.reference()]
                } else {
                    vec![]
                }
            }
            Pattern::Literal(_) | Pattern::Range { .. } | Pattern::Type(_) => vec![],
        }
    }

    pub fn precedence(&self, bindings: &[u16]) -> i32 {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.precedence(bindings);
                let right = right.precedence(bindings);
                if *operator == LogOp::And {
                    let highest = if left > right { left } else { right };

                    highest + 1
                } else {
                    let lowest = if left < right { left } else { right };

                    lowest - 1
                }
            }
            Pattern::Comparision { .. } => PatternPrecedence::Comparision.into(),
            Pattern::List { left, right } => {
                (left.precedence(bindings) + right.precedence(bindings)) / 2
            }
            Pattern::Literal(_) => PatternPrecedence::Literal.into(),
            Pattern::OperatorComparison { .. } => PatternPrecedence::Comparision.into(),
            Pattern::Range { .. } => PatternPrecedence::Range.into(),
            Pattern::Type(_) => PatternPrecedence::Type.into(),
            Pattern::Unary { right, .. } => {
                PatternPrecedence::COUNT as i32 * 10 - right.precedence(bindings)
            }
            Pattern::Wildcard(var) => {
                if let Some(var) = var {
                    if bindings.contains(&var.reference()) {
                        return PatternPrecedence::Literal.into();
                    }
                }

                PatternPrecedence::Wildcard.into()
            }
        }
    }

    pub fn is_binary(&self) -> bool {
        matches!(self, Pattern::Binary { .. })
    }

    pub fn is_comparison(&self) -> bool {
        matches!(self, Pattern::Comparision { .. })
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Pattern::List { .. })
    }

    pub fn is_literal(&self, bindings: &[Option<u16>]) -> bool {
        if let Pattern::Wildcard(name) = self {
            bindings.contains(&name.as_ref().map(|var| var.reference()))
        } else {
            matches!(self, Pattern::Literal(_))
        }
    }

    pub fn becomes_literal(&self) -> bool {
        if let Pattern::Binary {
            left,
            operator,
            right,
        } = self
        {
            left.is_literal(&[]) && right.is_literal(&[]) && *operator == LogOp::And
        } else {
            false
        }
    }

    pub fn is_operator_comparison(&self) -> bool {
        matches!(self, Pattern::OperatorComparison { .. })
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Pattern::Type(_))
    }

    pub fn is_wildcard(&self, bindings: &[Option<u16>]) -> bool {
        if let Pattern::Wildcard(name) = self {
            !bindings.contains(&name.as_ref().map(|var| var.reference()))
        } else {
            false
        }
    }

    pub fn becomes_wildcard(&self) -> bool {
        if let Pattern::Binary {
            left,
            operator,
            right,
        } = self
        {
            left.is_wildcard(&[]) && right.is_wildcard(&[]) && *operator == LogOp::Or
        } else {
            false
        }
    }
}

impl From<&Token> for Item {
    fn from(value: &Token) -> Self {
        match value.kind() {
            TokenType::Identifier => Item::Wildcard(Variable::Name(value.lexeme().to_owned())),
            TokenType::Nothing | TokenType::Number | TokenType::String => Item::Value(value.into()),
            _ => internal_error!("parsed token {:?} as literal or identifier", value),
        }
    }
}

impl From<Token> for Item {
    fn from(value: Token) -> Self {
        (&value).into()
    }
}

impl From<&Token> for Type {
    fn from(value: &Token) -> Self {
        match value.lexeme().as_str() {
            "Integer" => Type::Integer,
            "List" => Type::List,
            "Number" => Type::Number,
            "String" => Type::String,
            _ => internal_error!("parsed '{:?}' as type", value),
        }
    }
}

impl From<Token> for Type {
    fn from(value: Token) -> Self {
        (&value).into()
    }
}

impl From<&parser::Pattern> for Pattern {
    fn from(value: &parser::Pattern) -> Self {
        match value {
            parser::Pattern::Binary {
                left,
                operator,
                right,
            } => {
                let operator = match operator.kind() {
                    TokenType::Ampersand => LogOp::And,
                    TokenType::Pipe => LogOp::Or,
                    _ => internal_error!("parsed '{:?}' as pattern binary operator", operator),
                };

                Pattern::Binary {
                    left: Box::new((&**left).into()),
                    operator,
                    right: Box::new((&**right).into()),
                }
            }
            parser::Pattern::Comparision { comparison, rhs } => {
                let comparison = match comparison.kind() {
                    TokenType::EqualEqual => Comparision::Equal,
                    TokenType::BangEqual => Comparision::NotEqual,
                    TokenType::Greater => Comparision::Greater,
                    TokenType::GreaterEqual => Comparision::GreaterEqual,
                    TokenType::Less => Comparision::Less,
                    TokenType::LessEqual => Comparision::LessEqual,
                    _ => internal_error!("parsed '{:?}' as comparison", comparison),
                };

                Pattern::Comparision {
                    comparison,
                    rhs: rhs.into(),
                }
            }
            parser::Pattern::List { left, right } => Pattern::List {
                left: Box::new((&**left).into()),
                right: Box::new((&**right).into()),
            },
            parser::Pattern::Literal(token) => Pattern::Literal(token.into()),
            parser::Pattern::OperatorComparison {
                operator_chain,
                comparison,
                rhs,
            } => {
                let operator_chain = operator_chain.into();
                let comparison = match comparison.kind() {
                    TokenType::EqualEqual => Comparision::Equal,
                    TokenType::BangEqual => Comparision::NotEqual,
                    TokenType::Greater => Comparision::Greater,
                    TokenType::GreaterEqual => Comparision::GreaterEqual,
                    TokenType::Less => Comparision::Less,
                    TokenType::LessEqual => Comparision::LessEqual,
                    _ => internal_error!("parsed '{:?}' as comparison", comparison),
                };
                let rhs = rhs.into();
                Pattern::OperatorComparison {
                    operator_chain,
                    comparison,
                    rhs,
                }
            }
            parser::Pattern::Range {
                lower,
                upper,
                inclusive,
            } => Pattern::Range {
                lower: lower.into(),
                upper: upper.into(),
                inclusive: *inclusive,
            },
            parser::Pattern::Type(kind) => Pattern::Type(kind.into()),
            parser::Pattern::Unary { operator, right } => {
                let operator = match operator.kind() {
                    TokenType::Bang => UnOp::Not,
                    _ => internal_error!("parsed '{:?}' as pattern unary operator", operator),
                };

                Pattern::Unary {
                    operator,
                    right: Box::new((&**right).into()),
                }
            }
            parser::Pattern::Wildcard(token) => match token.kind() {
                TokenType::Identifier => {
                    Pattern::Wildcard(Some(Variable::Name(token.lexeme().to_string())))
                }
                TokenType::Underscore => Pattern::Wildcard(None),
                _ => internal_error!("parsed '{:?}' as wildcard", token),
            },
        }
    }
}

impl From<parser::Pattern> for Pattern {
    fn from(value: parser::Pattern) -> Self {
        (&value).into()
    }
}
