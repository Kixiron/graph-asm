use crate::Rewrite;
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn reductions() -> Vec<Rewrite> {
    vec![
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
    ]
}
