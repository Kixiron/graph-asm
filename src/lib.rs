mod const_eval;
mod cost;
mod operator;
mod rewrites;
mod types;

pub use const_eval::Constant;
pub use cost::OperatorCost;
pub use operator::{Func, Index, Input, Operator, Output};
pub use rewrites::rules;
pub use types::Type;

use egg::{Analysis, Id, Language};
use std::collections::HashMap;

type EGraph = egg::EGraph<Operator, OperatorAnalyzer>;
type Rewrite = egg::Rewrite<Operator, OperatorAnalyzer>;
type Pattern = egg::Pattern<Operator>;
#[cfg(test)]
type Runner = egg::Runner<Operator, OperatorAnalyzer>;
#[cfg(test)]
type RecExpr = egg::RecExpr<Operator>;

#[derive(Debug, Default)]
pub struct OperatorAnalyzer;

#[derive(Debug, Default)]
pub struct OperatorData {
    constant: Option<Constant>,
    ty: Option<Type>,
    decls: HashMap<Id, Id>,
}

impl Analysis<Operator> for OperatorAnalyzer {
    type Data = OperatorData;

    fn make(egraph: &EGraph, enode: &Operator) -> Self::Data {
        // FIXME: This doesn't propagate correctly
        let mut decls = HashMap::new();
        if let Operator::Let([var, val, _]) = *enode {
            decls.insert(var, val);
        }

        let ty = Type::typecheck(egraph, enode);
        let constant = Constant::evaluate(egraph, enode);

        OperatorData {
            constant,
            ty,
            decls,
        }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        let did_change = to.decls != from.decls;
        to.decls.extend(from.decls);

        if to.constant.is_none() && from.constant.is_some() {
            to.constant = from.constant;
            true
        } else {
            did_change
        }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        if let Some(constant) = egraph[id].data.constant.clone() {
            let constant = constant.as_operator(egraph);
            let const_id = egraph.add(constant);

            egraph.union(id, const_id);
        }

        // Union variable usages with the variable's value
        let decls = egraph[id].data.decls.clone();
        for node in egraph[id].nodes.clone() {
            for &child in node.children() {
                for child_node in egraph[child].nodes.clone() {
                    let new_child = egraph.add(child_node);
                    egraph[new_child].data.decls.extend(decls.clone());
                    egraph.union(child, new_child);
                }
            }

            if let Operator::Var(var) = node {
                debug_assert!(egraph[var].nodes.iter().all(Operator::is_symbol));

                if let Some(&body) = egraph[var].data.decls.get(&var) {
                    egraph.union(id, body);
                }
            }
        }
    }
}
