use crate::{rewrites::utils::is_numeric, Rewrite};
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn comparisons() -> Vec<Rewrite> {
    vec![
        // Comparisons are commutative
        ..rewrite!(
            "commutative-compare-eq";
            "(eq ?x ?y)" <=> "(eq ?y ?x)"
        ),
        ..rewrite!(
            "commutative-compare-neq";
            "(neq ?x ?y)" <=> "(neq ?y ?x)"
        ),
        ..rewrite!(
            "commutative-compare-less-greater";
            "(> ?x ?y)" <=> "(< ?y ?x)"
        ),
        ..rewrite!(
            "commutative-compare-less-eq-greater-eq";
            "(>= ?x ?y)" <=> "(<= ?y ?x)"
        ),

        rewrite!(
            "fuse-greater-compare-chain";
            "(or (> ?x ?y) (eq ?x ?y))" => "(>= ?x ?y)"
        ),
        rewrite!(
            "fuse-less-compare-chain";
            "(or (< ?x ?y) (eq ?x ?y))" => "(<= ?x ?y)"
        ),
        rewrite!(
            "fuse-greater-eq-chain";
            "(or (>= ?x ?y) (eq ?x ?y))" => "(>= ?x ?y)"
        ),
        rewrite!(
            "fuse-less-eq-chain";
            "(or (<= ?x ?y) (eq ?x ?y))" => "(<= ?x ?y)"
        ),

        rewrite!(
            "redundant-greater-comparison";
            "(or (> ?x ?y) (>= ?x ?y))" => "(>= ?x ?y)"
        ),
        rewrite!(
            "redundant-less-comparison";
            "(or (< ?x ?y) (<= ?x ?y))" => "(<= ?x ?y)"
        ),

        rewrite!(
            "evaluate-less-greater-than";
            "(or (< ?x ?y) (>= ?x ?y))" => "true"
                if is_numeric("?x")
                if is_numeric("?y")
        ),
        rewrite!(
            "evaluate-greater-less-than";
            "(or (> ?x ?y) (<= ?x ?y))" => "true"
                if is_numeric("?x")
                if is_numeric("?y")
        ),

        rewrite!(
            "redundant-or-eq-neq";
            "(or (eq ?x ?y) (neq ?x ?y))" => "true"
        ),
        rewrite!(
            "redundant-and-eq-neq";
            "(and (eq ?x ?y) (neq ?x ?y))" => "false"
        ),

        rewrite!(
            "contradictory-compare";
            "(and (> ?x ?y) (< ?x ?y))" => "false"
        ),

        // Comparing things against themselves is trivial
        rewrite!(
            "self-equality";
            "(eq ?x ?x)" => "true"
        ),
        rewrite!(
            "self-inequality";
            "(neq ?x ?x)" => "false"
        ),

        // Comparing things against negated versions of
        // themselves is trivial
        rewrite!(
            "eliminate-eq-not-comparison";
            "(eq ?x (not ?x))" => "false"
        ),
        rewrite!(
            "eliminate-neq-not-comparison";
            "(neq ?x (not ?x))" => "true"
        ),
        rewrite!(
            "eliminate-eq-neg-comparison";
            "(eq ?x (neg ?x))" => "false"
        ),
        rewrite!(
            "eliminate-neq-neg-comparison";
            "(neq ?x (neg ?x))" => "true"
        ),
    ]
}
