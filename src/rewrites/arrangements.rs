use crate::Rewrite;
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn arrangements() -> Vec<Rewrite> {
    vec![
        // Consolidation is too high-leveled of a construct for us
        // so we decompose it into its internal operations
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
    ]
}

#[cfg(test)]
mod tests {
    use crate::rewrites::rules;

    // https://github.com/vmware/differential-datalog/issues/926
    egg::test_fn! {
        redundant_consolidate_arrange,
        rules(),
        "(arrange_by_key (consolidate ?stream))"
            => "(arrange_by_key ?stream)"
    }

    egg::test_fn! {
        repeated_consolidate,
        rules(),
        "(consolidate (consolidate (consolidate ?stream)))"
            => "(consolidate ?stream)"
    }
}
