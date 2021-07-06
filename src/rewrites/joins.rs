use crate::{rewrites::utils::is_not_rev_tuple, Rewrite};
use egg::rewrite;
use velcro::vec;

#[rustfmt::skip]
pub fn joins() -> Vec<Rewrite> {
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

        // Fuse a map following a join into a join_map
        rewrite!(
            "fuse-join-maps";
            "(map (join ?arr1 ?arr2) ?map)"
                => "(join_map ?arr1 ?arr2 ?map)"
        ),
    ]
}
