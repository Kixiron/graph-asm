use crate::{EGraph, Operator, OperatorAnalyzer, Rewrite};
use egg::{rewrite, Applier, Id, Subst, Var};
use velcro::vec;

#[rustfmt::skip]
pub fn logical_expressions() -> Vec<Rewrite> {
    vec![
        // `and` and `or` are both commutative
        rewrite!(
            "commutative-and";
            "(and ?x ?y)" => "(and ?y ?x)"
        ),
        rewrite!(
            "commutative-or";
            "(or ?x ?y)" => "(or ?y ?x)"
        ),

        rewrite!(
            "distributive-and";
            "(and ?x (and ?y ?z))" => "(and ?z (and ?x ?y))"
        ),
        rewrite!(
            "distributive-or";
            "(or ?x (or ?y ?z))" => "(or ?z (or ?x ?y))"
        ),

        // An `and` or `or` with duplicate clauses is
        // reducible to a single invocation
        rewrite!(
            "simplify-duplicate-and";
            "(and ?x ?x)" => "?x"
        ),
        rewrite!(
            "simplify-duplicate-or";
            "(or ?x ?x)" => "?x"
        ),

        // Boolean operators against a negated condition are trivial
        rewrite!(
            "destructive-not-and";
            "(and ?x (not ?x))" => "false"
        ),
        rewrite!(
            "destructive-not-or";
            "(or ?x (not ?x))" => "true"
        ),

        // Logic operations with boolean literals are trivial to eliminate
        rewrite!(
            "eliminate-and-true";
            "(and ?cond true)" => "?cond"
        ),
        rewrite!(
            "eliminate-and-false";
            "(and ?cond false)" => "false"
        ),
        rewrite!(
            "eliminate-or-true";
            "(or ?cond true)" => "true"
        ),
        rewrite!(
            "eliminate-or-false";
            "(or ?cond false)" => "?cond"
        ),

        // Negating a negated value is a noop
        rewrite!(
            "redundant-not";
            "(not (not ?x))" => "?x"
        ),

        // Negation of conjunction
        ..rewrite!(
            "negation-of-conjunction";
            "(not (and ?x ?y))" <=> "(or (not ?x) (not ?y))"
        ),
        // Negation of disjunction
        ..rewrite!(
            "negation-of-disjunction";
            "(not (or ?x ?y))" <=> "(and (not ?x) (not ?y))"
        ),

        // if `?x = (not ?x)` then that node can be eliminated
        // since it's logically incoherent
        rewrite!(
            "eliminate-incoherent-not-chain";
            "(not ?x)" => { IncoherentNotChain::new("?x") }
        ),
    ]
}

#[derive(Debug)]
pub struct IncoherentNotChain {
    x: Var,
}

impl IncoherentNotChain {
    #[track_caller]
    pub fn new(x: &str) -> Self {
        Self {
            x: x.parse().unwrap(),
        }
    }
}

impl Applier<Operator, OperatorAnalyzer> for IncoherentNotChain {
    fn apply_one(&self, graph: &mut EGraph, eclass: Id, subst: &Subst) -> Vec<Id> {
        let eclass = graph.find(eclass);
        let x = graph.find(subst[self.x]);

        if graph[x].nodes.len() > 1 {
            graph[x].nodes.retain(|child| {
                if let Operator::Not(inner) = *child {
                    inner != eclass
                } else {
                    true
                }
            });
        }

        Vec::new()
    }

    fn vars(&self) -> Vec<Var> {
        vec![self.x]
    }
}
