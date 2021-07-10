use crate::Rewrite;
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn bitwise() -> Vec<Rewrite> {
    vec![
        // Bitwise And
        rewrite!(
            "commutative-bitand";
            "(bitand ?x ?y)" => "(bitand ?y ?x)"
        ),
        rewrite!(
            "associative-bitand";
            "(bitand ?x (bitand ?y ?z))"
                => "(bitand ?z (bitand ?x ?y))"
        ),
        rewrite!(
            "bitand-identity";
            "(bitand ?x ?x)" => "?x"
        ),
        // TODO: Need a more general "all bits set" thingy here
        //       to compose `(bitor ?x ALL_BITS) => ?x`
        rewrite!(
            "destructive-bitand";
            "(bitand ?x 0)" => "0"
        ),

        // Bitwise Or
        rewrite!(
            "commutative-bitor";
            "(bitor ?x ?y)" => "(bitor ?y ?x)"
        ),
        rewrite!(
            "associative-bitor";
            "(bitor ?x (bitor ?y ?z))"
                => "(bitor ?z (bitor ?x ?y))"
        ),
        rewrite!(
            "bitor-zero-identity";
            "(bitor ?x 0)" => "?x"
        ),
        // TODO: Need a more general "all bits set" thingy here
        //       to compose `(bitor ?x ALL_BITS) => ALL_BITS`

        // Xor
        rewrite!(
            "commutative-xor";
            "(xor ?x ?y)" => "(xor ?y ?x)"
        ),
        rewrite!(
            "associative-xor";
            "(xor ?x (xor ?y ?z))"
                => "(xor ?z (xor ?x ?y))"
        ),
        rewrite!(
            "bitxor-identity";
            "(xor ?x 0)" => "?x"
        ),
        rewrite!(
            "destructive-xor";
            "(xor ?x ?x)" => "0"
        ),
        rewrite!(
            "simplify-nested-xor";
            "(xor ?y (xor ?x ?y))"
                => "(xor ?x ?y)"
        ),
        // TODO: Need a more general "all bits set" thingy here
        //       to compose `(xor ?x ALL_BITS) <=> (not ?x)`

        // xor can be composed from bitand, bitor and bitwise not
        // and vice versa
        ..rewrite!(
            "xor-composed-of-bitand";
            "(xor ?x ?y)"
                <=> "(bitand (bitor ?x ?y) (bitor (not ?x) (not ?y)))"
        ),
        ..rewrite!(
            "xor-composed-of-bitor";
            "(xor ?x ?y)"
                <=> "(bitor (bitand ?x ?y) (bitand (not ?x) (not ?y)))"
        ),

        // Bit shift left
        rewrite!(
            "shl-identity";
            "(shl ?x 0)" => "?x"
        ),
        // TODO: Generalize this to powers of 2
        ..rewrite!(
            "shl-into-mul2";
            "(shl ?x 1)" <=> "(mul ?x 2)"
        ),
        // TODO: Generalize this to powers of 2
        ..rewrite!(
            "shl-into-mul3";
            "(add (shl ?x 1) ?x)" <=> "(mul ?x 3)"
        ),

        // Bit shift right
        rewrite!(
            "shr-identity";
            "(shr ?x 0)" => "?x"
        ),
        // TODO: Generalize this to powers of 2
        ..rewrite!(
            "shr-into-div2";
            "(shr ?x 1)" <=> "(div ?x 2)"
        ),

        rewrite!(
            "simplify-bitor-bitand";
            "(bitor ?x (bitand ?x ?y))" => "?x"
        ),
        rewrite!(
            "simplify-bitand-bitor";
            "(bitand ?x (bitor ?x ?y))" => "?x"
        ),

        ..rewrite!(
            "distributive-bitand-not";
            "(not (bitand ?x ?y))"
                <=> "(bitor (not ?x) (not ?y))"
        ),
        ..rewrite!(
            "distributive-bitor-not";
            "(not (bitor ?x ?y))"
                <=> "(bitand (not ?x) (not ?y))"
        ),
        ..rewrite!(
            "distributive-bitand-bitor";
            "(bitand ?x (bitor ?y ?z))"
                <=> "(bitor (bitand ?x ?y) (bitand ?x ?z))"
        ),
        ..rewrite!(
            "distributive-bitor-bitand";
            "(bitor ?x (bitand ?y ?z))"
                <=> "(bitand (bitor ?x ?y) (bitor ?x ?z))"
        ),
        ..rewrite!(
            "distributive-bitand-xor";
            "(bitand ?x (xor ?y ?z))"
                <=> "(xor (bitand ?x ?y) (bitand ?x ?z))"
        ),

        // Addition can be created with add, xor, bitand and shl
        // FIXME: Not like this it can't
        // ..rewrite!(
        //     "add-composed-of-bitwise";
        //     "(add ?x ?y)"
        //         <=> "(add (xor ?x ?y) (shl (bitand ?x ?y) 1))"
        // ),

        // Subtraction can be created with bitwise not and addition
        // FIXME: Not like this it can't
        // ..rewrite!(
        //     "sub-composed-of-not-add";
        //     "(sub ?x ?y)"
        //         <=> "(not (add (not ?x) ?y))"
        // ),

        // TODO: Redundant bitand after shift for all integer sizes
        //       (bitor (shl ?x 1) 0x00000001) => (shl ?x 1)
        //       (bitor (shl ?x 2) 0x00000011) => (shl ?x 2)
        //       (bitor (shr ?x 1) 0x10000000) => (shl ?x 1)
        // TODO: Shifting by the width of the integer gives zero
        //       (shl ?x bits_of("?x")) => 0
        //       (shr ?x bits_of("?x")) => 0
    ]
}

#[cfg(test)]
mod tests {
    use crate::rewrites::rules;

    egg::test_fn! {
        all_of_the_xor,
        rules(),
        "(xor ?x
            (xor ?y
                (xor ?y
                    (xor ?y
                        (xor ?y
                            (xor ?y
                                (xor ?y
                                    (xor ?y
                                        (xor ?y
                                            (xor ?y
                                                (xor ?y ?y)))))))))))"
        => "(xor ?x ?y)"
    }
}
