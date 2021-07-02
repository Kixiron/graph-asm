mod algebraic_laws;
mod logical_exprs;
mod tests;
mod utils;

use crate::{Pattern, Rewrite};
use algebraic_laws::algebraic_laws;
use egg::rewrite;
use logical_exprs::logical_expressions;
use utils::is_not_rev_tuple;
use velcro::vec;

#[rustfmt::skip]
pub fn rules() -> Vec<Rewrite> {
    vec![
        ..join_laws(),
        ..algebraic_laws(),
        ..logical_expressions(),
        ..empty_collections(),
        ..collapse_concatenation(),

        // Fuse two maps together
        rewrite!(
            "fuse-map";
            "(map (map ?stream ?map1) ?map2)"
                => "(map ?stream (fun (apply ?map2 (apply ?map1 #0))))"
        ),
        // Fuse two filters together
        rewrite!(
            "fuse-filter";
            "(filter (filter ?stream ?filter1) ?filter2)"
            =>
            "(filter ?stream
                (fun (and (apply ?filter1 #0)
                          (apply ?filter2 #0))))"
        ),

        // Fuse a map and filter into a filter_map
        rewrite!(
            "fuse-map-and-filter";
            "(map (filter ?stream ?filter) ?map)"
            =>
            "(filter_map ?stream
                (fun (if (apply ?filter #0)
                         (some (apply ?map #0))
                         none)))"
        ),
        // Fuse a filter and a map into a filter_map
        rewrite!(
            "fuse-filter-and-map";
            "(filter (map ?stream ?map) ?filter)"
            =>
            "(filter_map ?stream
                (fun (if (apply (apply ?filter (apply ?map #0)) #0)
                         (some (apply ?map #0))
                         none)))"
        ),

        // Fuse a filter and a filter_map into a single filter_map
        rewrite!(
            "fuse-map-filter-map";
            "(filter (filter_map ?stream ?filter_map) ?filter)"
            =>
            "(filter_map ?stream
                (fun (filter_opt (apply ?filter_map #0) ?filter)))"
        ),

        // Fuse two filter_maps together
        rewrite!(
            "fuse-filter-maps";
            "(filter_map (filter_map ?stream ?filter_map1) ?filter_map2)"
                => "(filter_map ?stream (and_then ?filter_map1 ?filter_map2))"
        ),

        // Fuse a map following a join into a join_map
        rewrite!(
            "fuse-join-maps";
            "(map (join ?arr1 ?arr2) ?map)"
                => "(join_map ?arr1 ?arr2 ?map)"
        ),

        // Fuse filters, maps and filter_maps following reductions into the reduction
        rewrite!(
            "fuse-reduce-map";
            "(map (reduce ?arr ?reduce) ?map)"
                => "(reduce ?arr (apply ?reduce ?map))"
        ),
        rewrite!(
            "fuse-reduce-filter";
            "(filter (reduce ?arr ?reduce) ?filter)"
                => "(reduce ?arr (filter_opt (some ?reduce) ?filter))"
        ),
        rewrite!(
            "fuse-reduce-filter-map";
            "(filter_map (reduce ?arr ?reduce) ?filter_map)"
                => "(reduce ?arr (and_then (some ?reduce) ?filter_map))"
        ),
        // Note: While we *could* fuse filters & maps from before a reduce into the reduce itself,
        //       we generally don't want to since not filtering and not mapping into a (hopefully)
        //       more refined data type will cause the arrangement size to grow. However, if we have
        //       a situation where the target collection already has an arrangement in existence
        //       followed by a filter and then the reduce (that is, `(reduce (filter ?arranged ?filter) ?reduce)`)
        //       then we can take advantage of this by filtering the arrangement instead of filtering
        //       the source collection, arranging it and then reducing it. We could also probably write
        //       a custom version of `differential_dataflow::trace::wrappers::filter::TraceFilter`
        //       that filters and maps values, our own `TraceFilterMap` and maybe even a `TraceMap`
        //       if that could possibly be worth it to avoid arrangements.

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
        rewrite!(
            "expand-arrange-by-self";
            "(arrange_by_self ?x)"
                => "(arrange_by_key (map ?x (fun (tuple #0 unit))))"
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
pub fn join_laws() -> Vec<Rewrite> {
    vec![
        // Joins are commutative
        rewrite!(
            "commutative-join";
            "(join ?x ?y)"
                => "(join_map ?y ?x (fun (rev_tuple #0)))"
        ),
        rewrite!(
            "simplify-commutative-join";
            "(join_map ?y ?x (fun (rev_tuple #0)))"
                => "(join ?x ?y)"
        ),

        rewrite!(
            "commutative-join-map";
            "(join_map ?x ?y ?map)"
                => "(join_map ?y ?x (fun (apply ?map (rev_tuple #0))))"
                // Only express commutativity if we haven't already done it
                // to this join, otherwise it'll cause exponential
                // blowup of adding and removing `rev_tuple`s
                if is_not_rev_tuple("?map")
        ),
        rewrite!(
            "simplify-commutative-join-map";
            "(join_map ?y ?x (fun (apply ?map (rev_tuple #0))))"
                => "(join_map ?x ?y ?map)"
        ),

        rewrite!(
            "commutative-join-filter";
            "(join_filter ?x ?y ?filter)"
            =>
            "(join_filter ?y ?x
                (fun (apply ?filter (rev_tuple #0))))"
            // Only express commutativity if we haven't already done it
            // to this join, otherwise it'll cause exponential
            // blowup of adding and removing `rev_tuple`s
            if is_not_rev_tuple("?filter")
        ),
        rewrite!(
            "simplify-commutative-join-filter";
            "(join_filter ?y ?x
                (fun (apply ?filter (rev_tuple #0))))"
                => "(join_filter ?x ?y ?filter)"
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
