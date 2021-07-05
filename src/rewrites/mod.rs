mod algebraic_laws;
mod comparisons;
mod filters_and_maps;
mod inline_functions;
mod joins;
mod logical_exprs;
mod reductions;
mod tests;
mod utils;

use crate::{Pattern, Rewrite};
use algebraic_laws::algebraic_laws;
use comparisons::comparisons;
use egg::rewrite;
use filters_and_maps::filters_and_maps;
use inline_functions::inline_functions;
use joins::joins;
use logical_exprs::logical_expressions;
use reductions::reductions;
use velcro::vec;

#[rustfmt::skip]
pub fn rules() -> Vec<Rewrite> {
    vec![
        ..joins(),
        ..algebraic_laws(),
        ..logical_expressions(),
        ..empty_collections(),
        ..collapse_concatenation(),
        ..comparisons(),
        inline_functions(),
        ..filters_and_maps(),
        ..reductions(),

        // Consolidation is too high-leveled of a construct for us
        // so we decompose it into its internal operations
        //
        // Note: To prevent extracting it, consolidation is heavily
        //       penalized within cost analysis
        rewrite!(
            "expand-consolidation";
            "(consolidate ?x)"
                => "(as_collection (arrange_by_key (map ?x (fun (tuple #0 unit)))) (fun #0))"
        ),

        // `arrange-by-self` is sugar for mapping by unit and arranging by key,
        // expand it to try and find some optimization opportunities with
        // fusing filters and/or maps
        ..rewrite!(
            "expand-arrange-by-self";
            "(arrange_by_self ?x)"
                <=> "(arrange_by_key (map ?x (fun (tuple #0 unit))))"
        ),

        // Remove redundant arrange->as_collection->arrange chains
        rewrite!(
            "eliminate-arrange_key-collection-arrange_key";
            "(arrange_by_key (as_collection (arrange_by_key ?stream) (fun #0)))"
                => "(arrange_by_key ?stream)"
        ),
        rewrite!(
            "eliminate-arrange_self-collection-arrange_self";
            "(arrange_by_self (as_collection (arrange_by_self ?stream) (fun #0)))"
                => "(arrange_by_self ?stream)"
        ),

        // Mapping by identity is a noop
        rewrite!(
            "remove-identity-map";
            "(map ?stream (fun #0))" => "?stream"
        ),
        rewrite!(
            "remove-identity-join-map";
            "(join_map ?x ?y (fun #0))" => "(join ?x ?y)"
        ),

        // A filter applied to the output of a filter_map can be fused
        // together by using the filter_opt function
        rewrite!(
            "fuse-filter-map-filter";
            "(filter (filter_map ?stream ?filter_map) ?filter)"
                => "(filter_map ?stream (filter_opt ?filter_map ?filter))"
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

        rewrite!(
            "eliminate-redundant-concat";
            "(concat ?stream)" => "?stream"
        ),
        rewrite!(
            "commutative-concatenation";
            "(concat ?stream1 ?stream2)"
                => "(concat ?stream2 ?stream1)"
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

        rewrite!(
            "eliminate-empty-concat/1";
            "(concat)" => "empty"
        ),
        // TODO: Make a variadic rewrite for this
        rewrite!(
            "eliminate-empty-concat/2";
            "(concat empty empty)" => "empty"
        ),
    ]
}

// Variadic concatenation was a mistake
// FIXME: This really sucks for a lot of reasons, make a custom rewrite for this
fn collapse_concatenation() -> Vec<Rewrite> {
    const COLLAPSE_LEVELS: usize = 25;

    let mut collapses = Vec::with_capacity(COLLAPSE_LEVELS * 2);
    for i in 1..=COLLAPSE_LEVELS {
        let name = format!("collapse-concat-start/{}", i);

        let streams = (1..=i)
            .map(|i| format!("?stream{}", i))
            .collect::<Vec<_>>()
            .join(" ");

        let from: Pattern = format!(
            "(concat (concat ?concat_stream1 ?concat_stream2) {})",
            streams,
        )
        .parse()
        .unwrap();
        let to: Pattern = format!("(concat ?concat_stream1 ?concat_stream2 {})", streams)
            .parse()
            .unwrap();

        collapses.push(rewrite!(
            name;
            from => to
        ));

        let name = format!("collapse-concat-end/{}", i);
        let from: Pattern = format!(
            "(concat {} (concat ?concat_stream1 ?concat_stream2))",
            streams,
        )
        .parse()
        .unwrap();
        let to: Pattern = format!("(concat ?concat_stream1 ?concat_stream2 {})", streams)
            .parse()
            .unwrap();

        collapses.push(rewrite!(
            name;
            from => to
        ));
    }

    collapses
}
