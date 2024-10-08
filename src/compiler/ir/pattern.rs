use crate::{
    common::{
        internal_error,
        value::{Type, Value},
    },
    compiler::{
        ir::Environment,
        parser,
        scanner::{Token, TokenType},
    },
    prelude::*,
};

use core::cmp::max;

use alloc::collections::VecDeque;

use strum::EnumCount;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Comparision {
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
pub(crate) enum LogOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Item {
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
pub(crate) struct OperatorChain {
    pub(crate) operator: ArithOp,
    pub(crate) rhs: Item,
    pub(crate) next: Option<Box<OperatorChain>>,
}

impl OperatorChain {
    fn bind(&mut self, environment: &mut Environment) {
        if let Item::Wildcard(Variable::Name(name)) = &self.rhs {
            let (reference, pre_set) = environment.get_or_add(name.to_owned());
            self.rhs = Item::Wildcard(Variable::Reference { reference, pre_set });
        }

        if let Some(next) = &mut self.next {
            next.bind(environment);
        }
    }

    fn references(&self, references: &mut Vec<u16>) {
        if let Item::Wildcard(Variable::Reference { reference, .. }) = &self.rhs {
            references.push(*reference);
        }

        if let Some(next) = &self.next {
            next.references(references);
        }
    }

    fn names<'a>(&'a self, names: &mut Vec<&'a String>) {
        if let Item::Wildcard(Variable::Name(name)) = &self.rhs {
            names.push(name);
        }

        if let Some(next) = &self.next {
            next.names(names);
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

        let rhs = value.mid().into();

        let next = value.next().map(|next| Box::new(next.into()));

        OperatorChain {
            operator,
            rhs,
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
pub(crate) enum Variable {
    Name(String),
    Reference { reference: u16, pre_set: bool },
}

impl Variable {
    pub(crate) fn reference(&self) -> u16 {
        if let Variable::Reference { reference, .. } = self {
            *reference
        } else {
            internal_error!("called 'reference' on a named variable");
        }
    }

    pub fn pre_set(&self) -> bool {
        if let Variable::Reference { pre_set, .. } = self {
            *pre_set
        } else {
            internal_error!("called 'pre_set' on a named variable");
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
    Concatenation(Vec<Item>),
    List {
        left: Box<Pattern>,
        right: Box<Pattern>,
    },
    Literal(Value),
    OperatorComparison {
        operator_chain: Option<OperatorChain>,
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
    Wildcard = 1,
    Type,
    Concatenation,
    Comparision,
    Range,
    Literal,
}

impl From<PatternPrecedence> for i32 {
    fn from(value: PatternPrecedence) -> Self {
        (value as u8 * 10).into()
    }
}

fn matches_concatenation(
    value: &Value,
    items: &[Item],
    mut variables: Vec<Value>,
) -> Option<Vec<Value>> {
    let string = value.as_string()?;
    let mut match_start = 0;
    let mut var_waiting = false;

    for item in items {
        match item {
            Item::Value(value) => {
                let current_string = value.expect_string();
                if let Some(index) = string.get(match_start..).unwrap_or("").find(current_string) {
                    if var_waiting {
                        variables.push(Value::String(string[match_start..index].to_owned()));
                        var_waiting = false;
                    } else if index != match_start {
                        return None;
                    }

                    match_start = index + current_string.len();
                } else {
                    return None;
                }
            }
            Item::Wildcard(var) => {
                if let Some(value) = variables.get(var.reference() as usize) {
                    let current_string = value.expect_string();
                    if let Some(index) =
                        string.get(match_start..).unwrap_or("").find(current_string)
                    {
                        let match_start_tmp = index + current_string.len();
                        variables.push(Value::String(string[match_start..index].to_owned()));
                        match_start = match_start_tmp;
                        var_waiting = false;
                    } else {
                        return None;
                    }
                } else if var_waiting {
                    internal_error!("sequential wildcards")
                } else {
                    var_waiting = true;
                }
            }
        }
    }

    if var_waiting {
        variables.push(Value::String(
            string.get(match_start..).unwrap_or("").to_owned(),
        ));
    } else if match_start < string.len() {
        return None;
    }

    Some(variables)
}

fn matches_operator_comparison(
    value: &Value,
    mut operator_chain: Option<&OperatorChain>,
    comparison: Comparision,
    rhs: &Item,
    variables: Vec<Value>,
) -> Option<Vec<Value>> {
    let mut value = value.to_owned();

    while let Some(operator) = operator_chain {
        let mid = operator.rhs.resolve(&variables)?.clone();

        value = match operator.operator {
            ArithOp::Add => value + mid,
            ArithOp::Sub => value - mid,
            ArithOp::Mul => value * mid,
            ArithOp::Div => value / mid,
            ArithOp::Mod => value % mid,
        }?;

        operator_chain = operator.next.as_deref();
    }

    let rhs = rhs.resolve(&variables)?;
    let matches = comparison.compare(&value, rhs);

    if matches {
        Some(variables)
    } else {
        None
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
                    let right = right.matches(value, variables);
                    left.map_or(right, Some)
                } else {
                    let variables = left.matches(value, variables)?;
                    let variables = right.matches(value, variables)?;
                    Some(variables)
                }
            }
            Pattern::Concatenation(items) => matches_concatenation(value, items, variables),
            Pattern::List { left, right } => {
                if let Value::List(list) = value {
                    let variables = left.matches(&list[0], variables)?;
                    let tail = match list.get(1) {
                        Some(_) => Value::List(VecDeque::from(list.as_slices().0.to_vec())),
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
            } => matches_operator_comparison(
                value,
                operator_chain.as_ref(),
                *comparison,
                rhs,
                variables,
            ),
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
                if kind.is_kind(value) {
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

    pub(super) fn bind(&mut self, environment: &mut Environment) {
        match self {
            Pattern::Binary { left, right, .. } | Pattern::List { left, right } => {
                left.bind(environment);
                right.bind(environment);
            }
            Pattern::Concatenation(items) => {
                for item in items {
                    if let Item::Wildcard(Variable::Name(name)) = item {
                        let (reference, pre_set) = environment.get_or_add(name.to_owned());
                        *item = Item::Wildcard(Variable::Reference { reference, pre_set });
                    }
                }
            }
            Pattern::OperatorComparison {
                operator_chain,
                rhs,
                ..
            } => {
                if let Some(operator_chain) = operator_chain {
                    operator_chain.bind(environment);
                }

                if let Item::Wildcard(Variable::Name(name)) = rhs {
                    let (reference, pre_set) = environment.get_or_add(name.to_owned());
                    *rhs = Item::Wildcard(Variable::Reference { reference, pre_set });
                }
            }
            Pattern::Unary { right, .. } => right.bind(environment),
            Pattern::Wildcard(binding) => {
                if let Some(Variable::Name(name)) = binding {
                    let (reference, pre_set) = environment.get_or_add(name.to_owned());
                    *self = Pattern::Wildcard(Some(Variable::Reference { reference, pre_set }));
                }
            }
            Pattern::Literal(_) | Pattern::Range { .. } | Pattern::Type(_) => {}
        }
    }

    pub fn references(&self) -> Vec<u16> {
        match self {
            Pattern::Binary { left, right, .. } | Pattern::List { left, right } => {
                let mut references = left.references();
                references.append(&mut right.references());
                references
            }
            Pattern::Concatenation(items) => items
                .iter()
                .filter_map(|item| {
                    if let Item::Wildcard(var) = item {
                        Some(var.reference())
                    } else {
                        None
                    }
                })
                .collect(),
            Pattern::OperatorComparison {
                operator_chain,
                rhs,
                ..
            } => {
                let mut references = vec![];
                if let Some(operator_chain) = operator_chain {
                    operator_chain.references(&mut references);
                }
                if let Item::Wildcard(var) = rhs {
                    references.push(var.reference());
                }
                references
            }
            Pattern::Unary { right, .. } => right.references(),
            Pattern::Wildcard(var) => var
                .as_ref()
                .map_or_else(Vec::new, |var| vec![var.reference()]),
            Pattern::Literal(_) | Pattern::Range { .. } | Pattern::Type(_) => vec![],
        }
    }

    pub fn names(&self) -> Vec<&String> {
        match self {
            Pattern::Binary { left, right, .. } | Pattern::List { left, right } => {
                let mut names = left.names();
                names.append(&mut right.names());
                names
            }
            Pattern::Concatenation(items) => items
                .iter()
                .filter_map(|item| {
                    if let Item::Wildcard(Variable::Name(name)) = item {
                        Some(name)
                    } else {
                        None
                    }
                })
                .collect(),
            Pattern::OperatorComparison {
                operator_chain,
                rhs,
                ..
            } => {
                let mut names = vec![];
                if let Some(operator_chain) = operator_chain {
                    operator_chain.names(&mut names);
                }
                if let Item::Wildcard(Variable::Name(name)) = rhs {
                    names.push(name);
                }
                names
            }
            Pattern::Unary { right, .. } => right.names(),
            Pattern::Wildcard(var) => {
                if let Some(Variable::Name(name)) = var {
                    vec![name]
                } else {
                    vec![]
                }
            }
            Pattern::Literal(_) | Pattern::Range { .. } | Pattern::Type(_) => vec![],
        }
    }

    pub fn precedence(&self) -> i32 {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.precedence();
                let right = right.precedence();
                if *operator == LogOp::And {
                    let highest = if left > right { left } else { right };

                    highest + 1
                } else {
                    let lowest = if left < right { left } else { right };

                    lowest - 1
                }
            }
            Pattern::Concatenation { .. } => PatternPrecedence::Concatenation.into(),
            Pattern::List { left, right } => max(
                (left.precedence() + right.precedence()) / 2,
                PatternPrecedence::Type.into(),
            ),
            Pattern::Literal(_) => PatternPrecedence::Literal.into(),
            Pattern::OperatorComparison { .. } => PatternPrecedence::Comparision.into(),
            Pattern::Range { .. } => PatternPrecedence::Range.into(),
            Pattern::Type(_) => PatternPrecedence::Type.into(),
            Pattern::Unary { right, .. } => PatternPrecedence::COUNT as i32 - right.precedence(),
            Pattern::Wildcard(var) => {
                if let Some(var) = var {
                    if var.pre_set() {
                        return PatternPrecedence::Literal.into();
                    }
                }

                PatternPrecedence::Wildcard.into()
            }
        }
    }

    pub fn is_literal(&self, bindings: &[Option<u16>]) -> bool {
        if let Pattern::Wildcard(name) = self {
            bindings.contains(&name.as_ref().map(Variable::reference))
        } else {
            matches!(self, Pattern::Literal(_))
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

fn convert_concatenation(start: &Token, operator_chain: &parser::OperatorChain) -> Pattern {
    let mut items = match start.kind() {
        TokenType::Identifier => {
            vec![Item::Wildcard(Variable::Name(start.lexeme().to_owned()))]
        }
        TokenType::String => vec![Item::Value(start.into())],
        _ => internal_error!("parsed {:?} as start of concatenation", start),
    };

    let mut chain = Some(operator_chain);

    while let Some(operator_chain) = chain {
        let item = match operator_chain.mid().kind() {
            TokenType::Identifier => {
                Item::Wildcard(Variable::Name(operator_chain.mid().lexeme().to_owned()))
            }
            TokenType::String => Item::Value(operator_chain.mid().into()),
            _ => internal_error!("parsed {:?} as concatenation item", operator_chain.mid()),
        };

        items.push(item);

        chain = operator_chain.next();
    }

    Pattern::Concatenation(items)
}

impl From<&parser::Pattern> for Pattern {
    fn from(value: &parser::Pattern) -> Self {
        use parser::Pattern as ParserPattern;

        match value {
            ParserPattern::Binary {
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
            ParserPattern::Concatenation {
                start,
                operator_chain,
            } => convert_concatenation(start, operator_chain),
            ParserPattern::List { left, right } => Pattern::List {
                left: Box::new((&**left).into()),
                right: Box::new((&**right).into()),
            },
            ParserPattern::Literal(token) => Pattern::Literal(token.into()),
            ParserPattern::OperatorComparison {
                operator_chain,
                comparison,
                rhs,
            } => {
                let operator_chain = operator_chain.as_ref().map(Into::into);
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
            ParserPattern::Range {
                lower,
                upper,
                inclusive,
            } => Pattern::Range {
                lower: lower.into(),
                upper: upper.into(),
                inclusive: *inclusive,
            },
            ParserPattern::Type(kind) => Pattern::Type(kind.into()),
            ParserPattern::Unary { operator, right } => {
                let operator = match operator.kind() {
                    TokenType::Bang => UnOp::Not,
                    _ => internal_error!("parsed '{:?}' as pattern unary operator", operator),
                };

                Pattern::Unary {
                    operator,
                    right: Box::new((&**right).into()),
                }
            }
            ParserPattern::Wildcard(token) => match token.kind() {
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
