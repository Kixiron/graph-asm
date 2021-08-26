mod algebraic_laws;
mod arrangements;
mod bitwise;
mod comparisons;
mod concatenation;
mod filters_and_maps;
mod inline_functions;
mod joins;
mod logical_exprs;
mod loops;
mod reductions;
mod tests;
mod utils;

use crate::Rewrite;
use egg::{rewrite, ConditionEqual};
use velcro::vec;

#[rustfmt::skip]
pub fn rules() -> Vec<Rewrite> {
    vec![
        ..joins::joins(),
        ..algebraic_laws::algebraic_laws(),
        ..logical_exprs::logical_expressions(),
        ..empty_collections(),
        ..comparisons::comparisons(),
        inline_functions::inline_functions(),
        ..filters_and_maps::filters_and_maps(),
        ..reductions::reductions(),
        ..concatenation::concatenation(),
        ..arrangements::arrangements(),
        ..bitwise::bitwise(),
        ..loops::loops(),

        // Mapping by identity is a noop
        rewrite!(
            "remove-identity-map";
            "(map ?stream (fun #0))" => "?stream"
        ),
        rewrite!(
            "remove-identity-join-map";
            "(join_map ?x ?y (fun #0))" => "(join ?x ?y)"
        ),

        // // Joining a collection on itself without changing the
        // // key is equivalent to mapping the value
        // rewrite!(
        //     "redundant-self-join";
        //     "(join ?stream ?stream)"
        //         => "(map ?stream (fun (tuple key val) (tuple key (tuple val val))))"
        // ),

        // Repeatedly arranging a collection is redundant
        rewrite!(
            "duplicate-arrange-by-key";
            "(arrange_by_key (arrange_by_key ?stream))"
                => "(arrange_by_key ?stream)"
        ),
        rewrite!(
            "duplicate-arrange-by-self";
            "(arrange_by_self (arrange_by_self ?stream))"
                => "(arrange_by_self ?stream)"
        ),

        // `(and_then ?x ?y)` where either clause is
        // `none` can be reduced to the `none`
        rewrite!(
            "eliminate-and-then-none";
            "(and_then none ?x)" => "none"
        ),
        rewrite!(
            "eliminate-and-then-none2";
            "(and_then ?x none)" => "none"
        ),

        // If the clause of an `if` statement is constant
        // it can be simplified
        rewrite!(
            "simplify-true-if";
            "(if true ?then ?else)" => "?then"
        ),
        rewrite!(
            "simplify-false-if";
            "(if false ?then ?else)" => "?else"
        ),

        // If both clauses of an if statement are identical then
        // the if can be removed entirely in favor of the body
        rewrite!(
            "simplify-duplicate-if";
            "(if ?cond ?then ?else)" => "?then"
                if ConditionEqual::parse("?then", "?else")
        ),

        ..rewrite!(
            "commutative-if";
            "(if ?cond ?then ?else)" <=> "(if (not ?cond) ?else ?then)"
        ),

        // Filtering against a constant `true` or filter-mapping
        // against a constant `some` is redundant and both can
        // be simplified into the input collection
        rewrite!(
            "always-true-filter";
            "(filter ?stream true)" => "?stream"
        ),
        rewrite!(
            "always-some-filter-map";
            "(filter_map ?stream (some ?map))" => "(map ?stream ?map)"
        ),

        // When filtering against a constant `false` or
        // filter-mapping against a constant `none` the
        // collection would produce zero values at runtime,
        // so these expressions can be simplified into nothing
        // but an `empty` collection
        rewrite!(
            "always-false-filter";
            "(filter ?stream false)" => "empty"
        ),
        rewrite!(
            "always-none-filter-map";
            "(filter_map ?stream none)" => "empty"
        ),

        // Simplify chained option filtering to use logical ands
        ..rewrite!(
            "fuse-option-filters";
            "(filter_opt (filter_opt ?option ?filter1) ?filter2)"
                <=> "(filter_opt ?option
                        (fun (and (apply ?filter1 #0)
                                  (apply ?filter2 #0))))"
        ),

        // Simplify redundant `rev_tuple` invocations
        rewrite!(
            "simplify-rev-tuple";
            "(rev_tuple (rev_tuple ?tuple))"
                => "?tuple"
        ),

        // Eliminate identity functions where possible
        rewrite!(
            "eliminate-apply-identity";
            "(apply (fun #0) ?value)"
                => "?value"
        ),

        // Turn filter_map invocations that return an option based
        // on a boolean condition into a filter
        rewrite!(
            "simplify-literal-filter-map-options";
            "(filter_map ?stream 
                (fun (if ?condition (some #0) none)))"
            => "(filter ?stream ?condition)"
        ),

        // Typed nodes can be unwrapped to their inner values
        rewrite!(
            "unwrap-typed-nodes";
            "(typed ?type ?value)" => "?value"
        ),
    ]
}

#[rustfmt::skip]
fn empty_collections() -> Vec<Rewrite> {
    vec![
        // Remove redundant expressions surrounding empty collections
        // Joins
        rewrite!(
            "eliminate-empty-join";
            "(join ?x empty)" => "empty"
        ),
        rewrite!(
            "eliminate-empty-join-map";
            "(join_map ?x empty ?map)" => "empty"
        ),
        rewrite!(
            "eliminate-empty-join-filter";
            "(join_filter ?x empty ?filter)" => "empty"
        ),
    
        // Mapping & filtering
        rewrite!(
            "eliminate-empty-map";
            "(map empty ?map)" => "empty"
        ),
        rewrite!(
            "eliminate-empty-filter";
            "(filter empty ?filter)" => "empty"
        ),
        rewrite!(
            "eliminate-empty-filter-map";
            "(filter_map empty ?filter)" => "empty"
        ),

        // Arranging
        rewrite!(
            "eliminate-empty-arrange-by-key";
            "(arrange_by_key empty)" => "empty"
        ),
        rewrite!(
            "eliminate-empty-arrange-by-self";
            "(arrange_by_self empty)" => "empty"
        ),
    ]
}
