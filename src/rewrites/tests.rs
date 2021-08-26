#![cfg(test)]

use crate::{cost::OperatorCost, hooks, rewrites::rules, RecExpr, Runner};
use egg::{Extractor, Id, StopReason};

egg::test_fn! {
    fuse_maps,
    rules(),
    "(map (map input0 func0) func1)"
        => "(map input0 (fun (apply func1 (apply func0 #0))))"
}

egg::test_fn! {
    fuse_filters,
    rules(),
    "(filter (filter input0 func0) func1)"
    =>
    "(filter ?stream
        (fun (and (apply ?filter1 #0)
                  (apply ?filter2 #0))))"
}

egg::test_fn! {
    simplify_and_then,
    rules(),
    "(and_then (and_then none true) none)" => "none"
}

egg::test_fn! {
    fuse_filter_map_with_filter,
    rules(),
    "(filter (filter_map input0 func0) func1)"
    =>
    "(filter_map input0
        (fun (filter_opt (apply func0 #0) func1)))"
}

egg::test_fn! {
    remove_redundant_arrange_by_keys,
    rules(),
    "(arrange_by_key
        (arrange_by_key
            (arrange_by_key input0)))"
    =>
    "(arrange_by_key input0)"
}

egg::test_fn! {
    remove_redundant_arrange_by_selfs,
    rules(),
    "(arrange_by_self (arrange_by_self (arrange_by_self input0)))"
        => "(arrange_by_self input0)"
}

egg::test_fn! {
    reversed_join_simplifies,
    rules(),
    "(join_map input1 input0
        (fun (rev_tuple #0)))"
    =>
    "(join input0 input1)"
}

egg::test_fn! {
    join_stays_simple,
    rules(),
    "(join input0 input1)"
        => "(join input0 input1)"
}

egg::test_fn! {
    reversed_join_map_simplifies,
    rules(),
    "(join_map input0 input1
        (fun (apply func0 (rev_tuple #0))))"
        => "(join_map input1 input0 func0)"
}

egg::test_fn! {
    join_map_stays_simple,
    rules(),
    "(join_map input1 input0 func0)"
        => "(join_map input1 input0 func0)"
}

egg::test_fn! {
    const_evaluation,
    rules(),
    "(eq (add 1 (sub 1000 900)) (add 1 (add 1 (add 1 (add 1 (add 1 (add 1 (add 1 1))))))))"
        => "false"
}

egg::test_fn! {
    merge_differently_named_functions,
    rules(),
    "(list (fun (add #0 1)) (fun (add #0 1)))"
        => "(list (fun (add #0 1)) (fun (add #0 1)))"
}

egg::test_fn! {
    adjacent_arrangements,
    rules(),
    "(arrange_by_key (as_collection (arrange_by_key ?x) (fun #0)))"
        => "(arrange_by_key ?x)"
}

#[test]
fn join_map_and_tuples_saturate() {
    let expr: RecExpr = "(join_map input0 input1 func0)".parse().unwrap();

    let runner = Runner::default()
        .with_iter_limit(3)
        .with_expr(&expr)
        .run(&rules());

    assert!(
        matches!(runner.stop_reason, Some(StopReason::Saturated)),
        "stopped with reason {:?}",
        runner.stop_reason,
    );
}

#[test]
fn join_filter_and_tuples_saturate() {
    let expr: RecExpr = "(join_filter input0 input1 func0)".parse().unwrap();

    let runner = Runner::default()
        .with_iter_limit(3)
        .with_expr(&expr)
        .run(&rules());

    assert!(
        matches!(runner.stop_reason, Some(StopReason::Saturated)),
        "stopped with reason {:?}",
        runner.stop_reason,
    );
}

#[test]
fn optimize_fibonacci() {
    #[rustfmt::skip]
    const FIBONACCI: &str = indoc::indoc! {"
        (let FibonacciFor input0
            (let NeedsFibonacci
                (concat (var FibonacciFor)
                        (filter_map (var FibonacciFor)
                                    (fun (if (>= #0 1)
                                             (some (sub #0 1))
                                             none)))

                        (filter_map (var FibonacciFor)
                                    (fun (if (>= #0 2)
                                             (some (sub #0 2))
                                             none))))

                (let FibonacciLoop
                    (map
                        (join
                            (arrange_by_key
                                (map
                                    (join (arrange_by_self (var NeedsFibonacci))
                                          (arrange_by_key (map (var FibonacciLoop)
                                                               (fun (sub #0 1)))))

                                    (fun (tuple (untuple 1 (untuple 1 #0)) (untuple 0 #0))))))

                            (arrange_by_key
                                (map
                                    (join (arrange_by_self (var NeedsFibonacci))
                                          (arrange_by_key (map (var FibonacciLoop)
                                                               (fun (sub #0 2)))))

                                    (fun (tuple (untuple 1 (untuple 1 #0)) (untuple 0 #0))))))

                        (fun (tuple (untuple 0 #0) (add (untuple 0 (untuple 1 #0)) (untuple 1 (untuple 1 #0))))))
                    unit)))
    "};

    let expr: RecExpr = FIBONACCI.parse().unwrap();

    let runner = Runner::default()
        .with_hook(|runner| {
            hooks::variable_propagation(&mut runner.egraph, &runner.roots);
            hooks::inline_unused_let(&mut runner.egraph, &runner.roots);
            hooks::typecheck(&mut runner.egraph, &runner.roots);

            Ok(())
        })
        .with_expr(&expr)
        .run(&rules());
    let (egraph, root) = (runner.egraph, runner.roots[0]);

    let mut extractor = Extractor::new(&egraph, OperatorCost::new(&egraph, &runner.roots));
    let (best_cost, best) = extractor.find_best(root);
    println!("{}: {}", best_cost, best.pretty(80));

    let mut ids: Vec<Id> = egraph.classes().map(|class| class.id).collect();
    ids.sort();

    println!("finished, reason: {:?}", runner.stop_reason);

    for id in ids {
        let node = &egraph[id];
        let data = &node.data;

        let mut nodes = node.nodes.clone();
        nodes.sort();

        print!("{}: {:?}", id, nodes);
        if let Some(ty) = data.ty.as_ref() {
            print!(", type: {:?}", ty);
        }
        if let Some(constant) = data.constant.as_ref() {
            print!(", constant: {:?}", constant);
        }
        if let Some(parity) = data.meta.parity.as_ref() {
            print!(", parity: {:?}", parity);
        }
        if let Some(is_power_of_two) = data.meta.is_power_of_two.as_ref() {
            print!(", is_power_of_two: {:?}", is_power_of_two);
        }
        println!();
    }
}

/// This module contains a bunch of code that ddlog currently generates
mod ddlog_generated_code {
    use crate::rewrites::rules;

    egg::test_fn! {
        /// ```rust
        /// pub fn a13(a: &i16, b: &i16) -> bool {
        ///     ((((((((&*a) < (&*b)) || ((&*a) > (&*b))) || ((&*a) <= (&*b))) || ((&*a) >= (&*b)))
        ///         || ((&*a) == (&*b)))
        ///         || ((&*a) != (&*b)))
        ///         || ((&*a) < (&*b)))
        /// }
        /// ```
        // TODO: Add a copy of this test that deals with the refs/derefs
        fuse_comparisons,
        rules(),
        "(or (<= ?a ?b)
         (or (eq ?a ?b)
             (or (< ?a ?b)
                 (or (neq ?a ?b)
                     (or (>= ?a ?b)
                         (or (< ?a ?b) (> ?a ?b)))))))"
        => "true"
    }

    egg::test_fn! {
        /// ```rust
        /// pub fn a10() -> i32 {
        ///     ((0 as i32).wrapping_add((1 as i32)))
        /// }
        /// ```
        // TODO: Add a copy of this that deals with the type casts?
        zero_plus_one,
        rules(),
        "(add 0 1)" => "1"
    }

    egg::test_fn! {
        /// ```rust
        /// pub fn a12(a: &i16, b: &i16) -> i16 {
        ///     ((((((*a).clone().wrapping_add((*b).clone()))
        ///         .wrapping_add(((*a).clone().wrapping_sub((*b).clone()))))
        ///     .wrapping_add(((*a).clone().wrapping_div((*b).clone()))))
        ///     .wrapping_add(((*a).clone().wrapping_mul((*b).clone()))))
        ///     .wrapping_add(((*a).clone().wrapping_rem((*b).clone()))))
        /// }
        /// ```
        // TODO: Add a copy of this that deals with the refs/derefs/clones
        chained_math,
        rules(),
        "(add 0 1)" => "1"
    }

    egg::test_fn! {
        /// ```rust
        /// pub fn a4() -> u32 {
        ///     ((((0 as u64) >> 9) & 0xffffffff) as u32)
        /// }
        /// ```
        // TODO: Add a copy of this that deals with the type casts?
        zero_bitwise_ops,
        rules(),
        "(bitand (shr 0 9) 4294967295)" => "0"
    }

    // TODO: Do this one
    // egg::test_fn! {
    //     /// ```rust
    //     /// pub fn a5(a: &u32, b: &u32) -> u32 {
    //     ///     ((((((((*a).clone() ^ (*b).clone()) | ((*a).clone() & (*b).clone()))
    //     ///         | ((*a).clone() | (*b).clone()))
    //     ///         | (!(*a).clone()))
    //     ///         | ((*a).clone().wrapping_shl((5 as u32))))
    //     ///         | ((*a).clone().wrapping_shr((5 as u32))))
    //     ///         | (((((((*a).clone() >> 0) & 0xffff) as u16) as u32) << 16)
    //     ///             | (((((*a).clone() >> 16) & 0xffff) as u16) as u32)))
    //     /// }
    //     /// ```
    //     // TODO: Add a copy of this that deals with the refs/derefs/clones
    //     chained_bitwise_ops,
    //     rules(),
    //     "" => ""
    // }

    egg::test_fn! {
        /// ```rust
        /// pub fn a51(a: &i32, b: &i32) -> i32 {
        ///     (((((((*a).clone() ^ (*b).clone()) | ((*a).clone() & (*b).clone()))
        ///         | ((*a).clone() | (*b).clone()))
        ///         | (!(*a).clone()))
        ///         | ((*a).clone().wrapping_shl((5 as u32))))
        ///         | ((*a).clone().wrapping_shr((5 as u32))))
        /// }
        /// ```
        // TODO: Add a copy of this that deals with the refs/derefs/clones
        chained_bitwise_ops2,
        rules(),
        // a = (untuple 0 #0)
        // b = (untuple 1 #0)
        "(fun
            (bitor
                (shr (untuple 0 #0) 5)
                (bitor 
                    (shl (untuple 0 #0) 5)
                    (bitor
                        (not (untuple 0 #0))
                        (bitor
                            (bitor
                                (xor (untuple 0 #0) (untuple 1 #0))
                                (bitand (untuple 0 #0) (untuple 1 #0)))
                            (bitor (untuple 0 #0) (untuple 1 #0)))))))"
        =>
        "(fun
            (bitor
                (shr (untuple 0 #0) 5)
                (bitor
                    (shl (untuple 0 #0) 5)
                    (bitor
                        (not (untuple 0 #0))
                        (bitor (untuple 0 #0) (untuple 1 #0))))))"
    }

    egg::test_fn! {
        /// ```rust
        /// pub fn a8() -> u32 {
        ///     ((((125 as u32) | (255 as u32)) | (511 as u32)) | (683 as u32))
        /// }
        /// ```
        // TODO: Add a copy of this that deals with the type casts?
        chained_bitwise_or,
        rules(),
        "(bitor (bitor (bitor 125 255) 511) 683)"
            => "1023"
    }
}
