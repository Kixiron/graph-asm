use crate::Rewrite;
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn loops() -> Vec<Rewrite> {
    vec![
        rewrite!(
            "unfold-false-loop";
            "(do_while ?body false)" => "?body"
        ),

        rewrite!(
            "reduce-duplicate-phi";
            // TODO: Make this variadic
            "(phi ?x ?x)" => "?x"
        ),
    ]
}

#[cfg(test)]
mod tests {
    use crate::rules;

    egg::test_fn! {
        unfold_false_loops,
        rules(),
        "(do_while ?body 1 == 2)"
            => "?body"
    }
}
