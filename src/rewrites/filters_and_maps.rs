use crate::Rewrite;
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn filters_and_maps() -> Vec<Rewrite> {
    vec![
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

        rewrite!(
            "fuse-as-collection-map";
            "(map (as_collection ?arrangement ?as_coll) ?map)"
                => "(as_collection ?arrangement (fun (apply ?map (apply ?as_coll #0))))"
        ),

        // A filter applied to the output of a filter_map can be fused
        // together by using the filter_opt function
        rewrite!(
            "fuse-filter-map-filter";
            "(filter (filter_map ?stream ?filter_map) ?filter)"
                => "(filter_map ?stream (filter_opt ?filter_map ?filter))"
        ),
    ]
}

#[cfg(test)]
mod tests {
    use crate::rewrites::rules;

    // https://github.com/vmware/differential-datalog/issues/925
    egg::test_fn! {
        maps_get_fused,
        rules(),
        "(map (map ?stream ?map1) ?map2)"
            => "(map ?stream (fun (apply ?map2 (apply ?map1 #0))))"
    }

    egg::test_fn! {
        as_collection_and_maps_get_fused,
        rules(),
        "(map (as_collection ?arranged ?as_coll) ?map)"
            => "(as_collection ?arranged (fun (apply ?map (apply ?as_coll #0))))"
    }

    // TODO: filter+filter, filter+map, map+filter, filter_map+map,
    //       filter_map+filter, filter_map+filter_map, etc.
}
