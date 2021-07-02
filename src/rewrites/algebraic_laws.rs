use crate::{rewrites::utils::is_numeric, Rewrite};
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn algebraic_laws() -> Vec<Rewrite> {
    vec![
        // Addition
        rewrite!(
            "commutative-add";
            "(add ?x ?y)" => "(add ?y ?x)"
        ),
        rewrite!(
            "associative-add";
            "(add ?x (add ?y ?z))" => "(add ?z (add ?x ?y))"
        ),
        ..rewrite!(
            "distributive-add";
            "(mul ?x (add ?y ?z))"
                <=> "(add (mul ?x ?y) (mul ?x ?z))"
        ),
        rewrite!(
            "add-identity";
            "(add ?x 0)" => "?x"
        ),
        rewrite!(
            "destructive-add";
            "(add ?x (neg ?x))" => "0"
        ),
        ..rewrite!(
            "add-self-into-mul";
            "(add ?x ?x)" <=> "(mul 2 ?x)"
        ),

        // Subtraction
        ..rewrite!(
            "distributive-sub";
            "(mul ?x (sub ?y ?z))"
                <=> "(sub (mul ?x ?y) (mul ?x ?z))"
        ),
        rewrite!(
            "sub-identity";
            "(sub ?x 0)" => "?x"
        ),
        rewrite!(
            "destructive-sub";
            "(sub ?x ?x)" => "0"
        ),
        rewrite!(
            "sub-zero";
            "(sub 0 ?x)" => "(neg ?x)"
        ),

        // Multiplication
        rewrite!(
            "commutative-mul";
            "(mul ?x ?y)" => "(mul ?y ?x)"
        ),
        rewrite!(
            "associative-mul";
            "(mul ?x (mul ?y ?z))" => "(mul ?z (mul ?x ?y))"
        ),
        rewrite!(
            "mul-identity";
            "(mul ?x 1)" => "?x"
        ),
        rewrite!(
            "destructive-mul";
            "(mul ?x 0)" => "0"
        ),

        // Division
        ..rewrite!(
            "distributive-div-add";
            "(div (add ?x ?y) ?z)"
                <=> "(add (div ?x ?z) (div ?y ?z))"
        ),
        ..rewrite!(
            "distributive-div-sub";
            "(div (sub ?x ?y) ?z)"
                <=> "(sub (div ?x ?z) (div ?y ?z))"
        ),
        rewrite!(
            "div-identity";
            "(div ?x 1)" => "?x"
        ),
        rewrite!(
            "destructive-div";
            "(div ?x ?x)" => "1"
                // Divisive destruction doesn't apply to floats
                if is_numeric("?x")
        ),

        rewrite!(
            "redundant-neg";
            "(neg (neg ?x))" => "?x"
        ),

        ..rewrite!(
            "commutative-compare";
            "(> ?x ?y)" <=> "(< ?y ?x)"
        ),
        ..rewrite!(
            "commutative-compare-eq";
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
            "redundant-or-eq-neq";
            "(or (eq ?x ?y) (neq ?x ?y))" => "true"
        ),
        rewrite!(
            "redundant-and-eq-neq";
            "(and (eq ?x ?y) (neq ?x ?y))" => "false"
        ),

        // Comparing against negated versions of things is trivial
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
