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
}

impl Analysis<Operator> for OperatorAnalyzer {
    type Data = OperatorData;

    fn make(egraph: &EGraph, enode: &Operator) -> Self::Data {
        let ty = Type::typecheck(egraph, enode);
        let constant = Constant::evaluate(egraph, enode);

        OperatorData { constant, ty }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        if to.constant.is_none() && from.constant.is_some() {
            to.constant = from.constant;
            true
        } else {
            false
        }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        if let Some(constant) = egraph[id].data.constant.clone() {
            let constant = constant.as_operator(egraph);
            let const_id = egraph.add(constant);

            egraph.union(id, const_id);
        }
    }
}
