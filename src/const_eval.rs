use std::ops::Neg;

use crate::{operator::Operator, EGraph};
use egg::Id;

#[derive(Debug, Clone)]
pub enum Constant {
    Unit,
    Int(i64),
    UInt(u64),
    Bool(bool),
    // List(Vec<Constant>),
    // Tuple(Vec<Constant>),
    Option(Option<Box<Self>>),
}

impl Constant {
    pub const fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(boolean) = *self {
            Some(boolean)
        } else {
            None
        }
    }

    pub fn is_true(&self) -> Option<bool> {
        if let Self::Bool(boolean) = *self {
            Some(boolean)
        } else {
            None
        }
    }

    pub fn evaluate(egraph: &EGraph, enode: &Operator) -> Option<Self> {
        let x = |&id: &Id| egraph[id].data.constant.clone();

        match enode {
            // Evaluate what expressions we can
            Operator::And([lhs, rhs]) => Some(Self::Bool(x(lhs)?.as_bool()? && x(rhs)?.as_bool()?)),
            Operator::Or([lhs, rhs]) => Some(Self::Bool(x(lhs)?.as_bool()? || x(rhs)?.as_bool()?)),

            Operator::Add([lhs, rhs]) => x(lhs)?.add(&x(rhs)?),
            Operator::Sub([lhs, rhs]) => x(lhs)?.sub(&x(rhs)?),
            Operator::Mul([lhs, rhs]) => x(lhs)?.mul(&x(rhs)?),
            Operator::Div([lhs, rhs]) => x(lhs)?.div(&x(rhs)?),

            Operator::Neg(val) => x(val)?.neg(),
            Operator::Not(val) => x(val)?.not(),

            Operator::Eq([lhs, rhs]) => Some(Self::Bool(x(lhs)?.eq(&x(rhs)?)?)),
            Operator::NotEq([lhs, rhs]) => Some(Self::Bool(x(lhs)?.not_eq(&x(rhs)?)?)),
            Operator::GreaterEq([lhs, rhs]) => Some(Self::Bool(x(lhs)?.greater_eq(&x(rhs)?)?)),
            Operator::Greater([lhs, rhs]) => Some(Self::Bool(x(lhs)?.greater(&x(rhs)?)?)),
            Operator::LessEq([lhs, rhs]) => Some(Self::Bool(x(lhs)?.less_eq(&x(rhs)?)?)),
            Operator::Less([lhs, rhs]) => Some(Self::Bool(x(lhs)?.less(&x(rhs)?)?)),

            // TODO: This may actually get covered by the standard rewrite rules
            Operator::FilterOption([optional, filter]) => {
                if x(filter)?.is_true()? {
                    x(optional)
                } else {
                    Some(Self::Option(None))
                }
            }

            // TODO: This may actually get covered by the standard rewrite rules
            Operator::If([cond, then, otherwise]) => {
                if x(cond)?.is_true()? {
                    x(then)
                } else {
                    x(otherwise)
                }
            }

            // Return constants directly
            Operator::Unit => Some(Self::Unit),
            Operator::None => Some(Self::Option(None)),
            // FIXME: Figure out how to get the inner value of a `Some` enode
            Operator::Some(inner) => x(inner).map(|inner| Self::Option(Some(Box::new(inner)))),
            &Operator::Int(int) => Some(Self::Int(int)),
            &Operator::UInt(uint) => Some(Self::UInt(uint)),
            &Operator::Bool(boolean) => Some(Self::Bool(boolean)),

            // TODO
            Operator::Index(_)
            | Operator::Case(_)
            | Operator::Apply(_)
            | Operator::AndThen(_)
            | Operator::ReverseTuple(_)
            | Operator::Func(_)
            | Operator::Let(_)
            | Operator::Var(_)
            | Operator::List(_)
            | Operator::Tuple(_) => None,

            // Symbols don't really mean anything, only
            // variable uses do
            Operator::Symbol(_) => None,

            // We can't statically evaluate dataflow operators
            // TODO: Or can we...
            Operator::Input(_)
            | Operator::Output(_)
            | Operator::OpaqueFunc(_)
            | Operator::Empty
            | Operator::Map(_)
            | Operator::Filter(_)
            | Operator::FilterMap(_)
            | Operator::Join(_)
            | Operator::JoinMap(_)
            | Operator::JoinFilter(_)
            | Operator::Reduce(_)
            | Operator::ArrangeByKey(_)
            | Operator::ArrangeBySelf(_)
            | Operator::Concat(_)
            | Operator::Consolidate(_)
            | Operator::AsCollection(_) => None,
        }
    }

    pub fn neg(&self) -> Option<Self> {
        match *self {
            Self::Int(val) => Some(Self::Int(val.neg())),

            _ => None,
        }
    }

    pub fn not(&self) -> Option<Self> {
        match *self {
            Self::Int(val) => Some(Self::Int(!val)),
            Self::UInt(val) => Some(Self::UInt(!val)),
            Self::Bool(val) => Some(Self::Bool(!val)),

            _ => None,
        }
    }

    pub fn add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(Self::Int(lhs.checked_add(rhs)?)),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(Self::UInt(lhs.checked_add(rhs)?)),

            (_, _) => None,
        }
    }

    pub fn sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(Self::Int(lhs.checked_sub(rhs)?)),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(Self::UInt(lhs.checked_sub(rhs)?)),

            (_, _) => None,
        }
    }

    pub fn mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(Self::Int(lhs.checked_mul(rhs)?)),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(Self::UInt(lhs.checked_mul(rhs)?)),

            (_, _) => None,
        }
    }

    pub fn div(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(Self::Int(lhs.checked_div(rhs)?)),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(Self::UInt(lhs.checked_div(rhs)?)),

            (_, _) => None,
        }
    }

    pub fn eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(true),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs == rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs == rhs),
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs == rhs),

            (Self::Option(None), Self::Option(None)) => Some(true),
            (Self::Option(None), Self::Option(Some(_)))
            | (Self::Option(Some(_)), Self::Option(None)) => Some(false),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.eq(rhs),

            (_, _) => None,
        }
    }

    pub fn not_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(false),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs != rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs != rhs),
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs != rhs),

            (Self::Option(None), Self::Option(None)) => Some(false),
            (Self::Option(None), Self::Option(Some(_)))
            | (Self::Option(Some(_)), Self::Option(None)) => Some(true),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.not_eq(rhs),

            (_, _) => None,
        }
    }

    pub fn greater_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(true),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs >= rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs >= rhs),
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs >= rhs),

            (Self::Option(None), Self::Option(Some(_))) => Some(false),
            (Self::Option(Some(_)), Self::Option(None)) => Some(true),
            (Self::Option(None), Self::Option(None)) => Some(true),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.greater_eq(rhs),

            (_, _) => None,
        }
    }

    pub fn greater(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(true),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs > rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs > rhs),
            #[allow(clippy::bool_comparison)]
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs > rhs),

            (Self::Option(None), Self::Option(Some(_)))
            | (Self::Option(None), Self::Option(None)) => Some(false),
            (Self::Option(Some(_)), Self::Option(None)) => Some(true),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.greater(rhs),

            (_, _) => None,
        }
    }

    pub fn less_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(true),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs <= rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs <= rhs),
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs <= rhs),

            (Self::Option(None), Self::Option(Some(_))) => Some(false),
            (Self::Option(Some(_)), Self::Option(None)) => Some(true),
            (Self::Option(None), Self::Option(None)) => Some(true),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.less_eq(rhs),

            (_, _) => None,
        }
    }

    pub fn less(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(true),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs < rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs < rhs),
            #[allow(clippy::bool_comparison)]
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs < rhs),

            (Self::Option(None), Self::Option(Some(_)))
            | (Self::Option(None), Self::Option(None)) => Some(false),
            (Self::Option(Some(_)), Self::Option(None)) => Some(true),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.less(rhs),

            (_, _) => None,
        }
    }

    pub fn as_operator(&self, egraph: &mut EGraph) -> Operator {
        match self {
            Self::Unit => Operator::Unit,
            &Self::Int(int) => Operator::Int(int),
            &Self::UInt(uint) => Operator::UInt(uint),
            &Self::Bool(boolean) => Operator::Bool(boolean),
            Self::Option(None) => Operator::None,
            Self::Option(Some(inner)) => {
                let inner = inner.as_operator(egraph);
                let inner = egraph.add(inner);

                Operator::Some(inner)
            }
        }
    }
}
