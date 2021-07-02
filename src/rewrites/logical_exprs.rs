use crate::Rewrite;
use egg::rewrite;
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
    ]
}
