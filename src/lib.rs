mod const_eval;
mod cost;
pub mod hooks;
mod operator;
mod rewrites;
mod types;

pub use const_eval::Constant;
pub use cost::OperatorCost;
pub use operator::{Func, Index, Input, Operator, Output};
pub use rewrites::rules;
pub use types::Type;

use egg::{Analysis, Id};
use std::collections::HashSet;

// type Pattern = egg::Pattern<Operator>;
type RecExpr = egg::RecExpr<Operator>;
type EGraph = egg::EGraph<Operator, OperatorAnalyzer>;
type Runner = egg::Runner<Operator, OperatorAnalyzer>;
type Rewrite = egg::Rewrite<Operator, OperatorAnalyzer>;

#[derive(Debug, Default)]
pub struct OperatorAnalyzer {
    contains_non_opaque_function_call: HashSet<Id>,
}

#[derive(Debug, Default)]
pub struct OperatorData {
    constant: Option<Constant>,
    ty: Option<Type>,
    meta: Metadata,
}

impl Analysis<Operator> for OperatorAnalyzer {
    type Data = OperatorData;

    fn make(graph: &EGraph, node: &Operator) -> Self::Data {
        let ty = Type::typecheck(graph, node);
        let constant = Constant::evaluate(graph, node);
        let meta = Metadata::collect(graph, node, constant.as_ref());

        OperatorData { constant, ty, meta }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        // TODO: Merge metadata

        if to.constant.is_none() && from.constant.is_some() {
            to.constant = from.constant;
            true
        } else {
            false
        }
    }

    fn modify(graph: &mut EGraph, id: Id) {
        if let Some(constant) = graph[id].data.constant.clone() {
            let constant = constant.as_operator(graph);
            let const_id = graph.add(constant);

            graph.union(id, const_id);
        }
    }
}

#[derive(Debug, Default)]
pub struct Metadata {
    parity: Option<Parity>,
    is_power_of_two: Option<bool>,
}

/// The parity of a value, whether its odd or even
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Parity {
    /// Even is defined as { 2𝑘 : 𝑘 ∈ ℤ }
    Even,
    /// Odd is defined as { 2𝑘 + 1 : 𝑘 ∈ ℤ }
    Odd,
}

impl Parity {
    /// Returns [`Parity::Even`]
    pub const fn even() -> Self {
        Self::Even
    }

    /// Returns [`Parity::Odd`]
    pub const fn odd() -> Self {
        Self::Odd
    }

    /// Returns `true` if the parity is [`Parity::Even`]
    pub const fn is_even(&self) -> bool {
        matches!(self, Self::Even)
    }

    /// Returns `true` if the parity is [`Parity::Odd`]
    pub const fn is_odd(&self) -> bool {
        matches!(self, Self::Odd)
    }

    /// Returns [`Parity::Even`] if `even_if` is `true`
    pub const fn even_if(even_if: bool) -> Self {
        if even_if {
            Self::even()
        } else {
            Self::odd()
        }
    }

    /// Returns [`Parity::Odd`] if `odd_if` is `true`
    pub const fn odd_if(odd_if: bool) -> Self {
        if odd_if {
            Self::odd()
        } else {
            Self::even()
        }
    }
}

impl Metadata {
    pub fn collect(graph: &EGraph, node: &Operator, constant: Option<&Constant>) -> Self {
        let x = |&id| &graph[id].data.meta;
        let mut meta = Self::default();

        match node {
            &Operator::Int(int) => {
                meta.parity = Some(Parity::even_if(int.rem_euclid(2) == 0));
                meta.is_power_of_two = Some(int.abs().count_ones() == 1);
            }

            &Operator::UInt(uint) => {
                meta.parity = Some(Parity::even_if(uint.rem_euclid(2) == 0));
                meta.is_power_of_two = Some(uint.is_power_of_two());
            }

            Operator::Add([lhs, rhs]) | Operator::Sub([lhs, rhs]) => {
                // x ± x = even
                if lhs == rhs {
                    meta.parity = Some(Parity::even());
                } else if let (Some(lhs), Some(rhs)) = (x(lhs).parity, x(rhs).parity) {
                    match (lhs.is_even(), rhs.is_even()) {
                        // even ± even = even, odd ± odd = even
                        (true, true) | (false, false) => {
                            meta.parity = Some(Parity::even());
                        }

                        // even ± odd = odd
                        (true, false) | (false, true) => {
                            meta.parity = Some(Parity::odd());
                        }
                    }
                }
            }

            Operator::Mul([lhs, rhs]) => {
                // x * x = x² = even
                if lhs == rhs {
                    meta.parity = Some(Parity::even());
                } else if let (Some(lhs), Some(rhs)) = (x(lhs).parity, x(rhs).parity) {
                    match (lhs, rhs) {
                        // odd * odd = odd
                        (Parity::Odd, Parity::Odd) => meta.parity = Some(Parity::odd()),

                        // even * even = even, even * odd = even
                        (Parity::Even, Parity::Even)
                        | (Parity::Even, Parity::Odd)
                        | (Parity::Odd, Parity::Even) => meta.parity = Some(Parity::even()),
                    }
                }
            }

            // TODO
            Operator::Shl(_)
            | Operator::Shr(_)
            | Operator::BitXor(_)
            | Operator::BitOr(_)
            | Operator::BitAnd(_)
            | Operator::Apply(_)
            | Operator::AndThen(_)
            | Operator::FilterOption(_)
            | Operator::ReverseTuple(_)
            | Operator::If(_)
            | Operator::Func(_)
            | Operator::Let(_)
            | Operator::Var(_)
            | Operator::Some(_)
            | Operator::None
            | Operator::List(_)
            | Operator::Tuple(_)
            | Operator::Case(_)
            | Operator::Unit
            | Operator::And(_)
            | Operator::Or(_)
            | Operator::Not(_)
            | Operator::Div(_)
            | Operator::Neg(_)
            | Operator::Eq(_)
            | Operator::NotEq(_)
            | Operator::GreaterEq(_)
            | Operator::Greater(_)
            | Operator::LessEq(_)
            | Operator::Less(_)
            | Operator::Index(_)
            | Operator::Bool(_)
            | Operator::Symbol(_)
            | Operator::Input(_)
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
            | Operator::AsCollection(_) => {}
        }

        // If this class has a constant value we can extract metadata from it
        if let Some(constant) = constant {
            match constant {
                &Constant::Int(int) => {
                    meta.parity = Some(Parity::even_if(int.rem_euclid(2) == 0));
                    meta.is_power_of_two = Some(int.abs().count_ones() == 1);
                }

                Constant::UInt(uint) => {
                    meta.parity = Some(Parity::even_if(uint.rem_euclid(2) == 0));
                    meta.is_power_of_two = Some(uint.is_power_of_two());
                }

                Constant::Bool(_) | Constant::Option(_) | Constant::Func(_) | Constant::Unit => {}
            }
        }

        meta
    }
}
