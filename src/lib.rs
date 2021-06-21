use egg::{rewrite, Analysis, EGraph, Id, Rewrite, Subst};
#[cfg(test)]
use egg::{RecExpr, Runner, StopReason};
use std::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};
use velcro::vec;

egg::define_language! {
    pub enum Operator {
        // An input operator
        Input(Input),
        // An output operator
        Output(Output),

        // An opaque function
        Func(Func),

        // An empty collection with no values
        "empty" = Empty,

        "map" = Map([Id; 2]),
        "filter" = Filter([Id; 2]),
        "filter_map" = FilterMap([Id; 2]),

        // `(join ?arr1 ?arr2)`
        "join" = Join([Id; 2]),

        // `(join_map ?arr1 ?arr2 ?map)` where `?map`
        // is a post-join map function
        "join_map" = JoinMap([Id; 3]),

        // `(join_filter ?arr1 ?arr2 ?filter)` where `?filter`
        // is a post-join filter function
        "join_filter" = JoinFilter([Id; 3]),

        "arrange_by_key" = ArrangeByKey(Id),
        "arrange_by_self" = ArrangeBySelf(Id),

        // `(apply ?func1 ?func2)`
        "apply" = Apply([Id; 2]),

        // An `.and_then()` combinator applied to `Option`s
        "and_then" = AndThen([Id; 2]),

        // `(filter_opt ?option ?predicate)`
        // A version of `Option::filter()` that returns `None`
        // when the predicate is false and the given value
        // when true
        "filter_opt" = FilterOption([Id; 2]),

        // `(rev_tuple ?tuple)`
        // Reverses the order of values in a tuple,
        // turning `(?x, ?y)` into `(?y, ?x)`
        "rev_tuple" = ReverseTuple(Id),

        // `(if ?cond ?then ?else)`
        "if" = If([Id; 3]),

        // `(and ?cond1 ?cond2)`
        "and" = And([Id; 2]),
        // `(or ?cond1 ?cond2)`
        "or" = Or([Id; 2]),

        "some" = Some(Id),
        "none" = None,

        // The identity function
        "identity" = Identity,

        // TODO: Variable bindings

        Bool(bool),
    }
}

#[derive(Debug, Default)]
pub struct OperatorAnalyzer {}

#[derive(Debug, Default)]
pub struct OperatorData {}

impl Analysis<Operator> for OperatorAnalyzer {
    type Data = OperatorData;

    fn make(_egraph: &EGraph<Operator, Self>, _enode: &Operator) -> Self::Data {
        OperatorData::default()
    }

    fn merge(&self, _to: &mut Self::Data, _from: Self::Data) -> bool {
        false
    }
}

#[rustfmt::skip]
pub fn rules() -> Vec<Rewrite<Operator, OperatorAnalyzer>> {
    vec![
        // Joins are commutative
        ..rewrite!(
            "commutative-join";
            "(join ?x ?y)" <=> "(join_map ?y ?x (rev_tuple identity))"
        ),
        rewrite!(
            "commutative-join-map";
            "(join_map ?x ?y ?map)"
                => "(join_map ?y ?x (rev_tuple ?map))"
                // Only express commutativity if we haven't already done it
                // to this join, otherwise it'll cause exponential
                // blowup of adding and removing `rev_tuple`s
                if is_not_rev_tuple("?map")
        ),
        rewrite!(
            "commutative-join-filter";
            "(join_filter ?x ?y ?filter)"
                => "(join_filter ?y ?x (rev_tuple ?filter))"
                // Only express commutativity if we haven't already done it
                // to this join, otherwise it'll cause exponential
                // blowup of adding and removing `rev_tuple`s
                if is_not_rev_tuple("?filter")
        ),

        // Fuse two maps together
        rewrite!(
            "fuse-map";
            "(map (map ?stream ?map1) ?map2)"
                => "(map ?stream (apply ?map1 ?map2))"
        ),
        // Fuse two filters together
        rewrite!(
            "fuse-filter";
            "(filter (filter ?stream ?filter1) ?filter2)"
                => "(filter ?stream (and ?filter1 ?filter2))"
        ),

        // Fuse a map and filter into a `filter_map`
        rewrite!(
            "fuse-map-and-filter";
            "(map (filter ?stream ?filter) ?map)"
                => "(filter_map ?stream (if ?filter (some ?map) none))"
        ),
        // Fuse a filter and a map into a `filter_map`
        rewrite!(
            "fuse-filter-and-map";
            "(filter (map ?stream ?map) ?filter)"
                => "(filter_map ?stream (filter_opt (some ?map) ?filter))"
        ),
        //  // Fuse two fused filter maps together
        //  rewrite!(
        //      "fuse-generated-filter-maps";
        //      "(filter_map (filter_map ?stream (if ?filter1 (some ?map) none)) (filter_opt ?option ?filter2))"
        //          => "(filter_map ?stream (if ?filter1 ()))"
        //          if is_some("?option")
        //  ),

        // Fuse two `filter_map`s together
        rewrite!(
            "fuse-filter-maps";
            "(filter_map (filter_map ?stream ?filter_map1) ?filter_map2)"
                => "(filter_map ?stream (and_then ?filter_map1 ?filter_map2))"
        ),

        // Fuse a `map` following a `join` into a `join_map`
        rewrite!(
            "fuse-join-maps";
            "(map (join ?arr1 ?arr2) ?map)"
                => "(join_map ?arr1 ?arr2 ?map)"
        ),

        // Mapping by identity is a noop
        rewrite!(
            "remove-identity-map";
            "(map ?stream identity)" => "?stream"
        ),
        rewrite!(
            "remove-identity-join-map";
            "(join_map ?x ?y identity)" => "(join ?x ?y)"
        ),

        // A filter applied to the output of a filter_map can be fused
        // together by using the filter_opt function
        rewrite!(
            "fuse-filter-map-filter";
            "(filter (filter_map ?stream ?filter_map) ?filter)"
                => "(filter_map ?stream (filter_opt ?filter_map ?filter))"
        ),

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

        // `and` and `or` are both commutative
        rewrite!(
            "commutative-and";
            "(and ?x ?y)" => "(and ?y ?x)"
        ),
        rewrite!(
            "commutative-or";
            "(or ?x ?y)" => "(or ?y ?x)"
        ),

        // An `and` or `or` with duplicate clauses is
        // reducible to a single invocation
        rewrite!(
            "simplify-duplicate-and";
            "(and ?x ?x)" => "?x"
        ),
        rewrite!(
            "simplify-duplicate-or";
            "(or ?x ?x)" => "?x"
        ),

        // Logic operations with boolean literals are trivial to eliminate
        rewrite!(
            "eliminate-and-true";
            "(and ?cond true)" => "?cond"
        ),
        rewrite!(
            "eliminate-and-false";
            "(and ?cond false)" => "false"
        ),
        rewrite!(
            "eliminate-or-true";
            "(or ?cond true)" => "true"
        ),
        rewrite!(
            "eliminate-or-false";
            "(or ?cond false)" => "?cond"
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

        // Simplify chained option filtering to use logical ands
        rewrite!(
            "fuse-option-filters";
            "(filter_opt (filter_opt ?option ?filter1) ?filter2)"
                => "(filter_opt ?option (and ?filter1 ?filter2))"
        ),

        // Simplify redundant `rev_tuple` invocations
        rewrite!(
            "simplify-rev-tuple";
            "(rev_tuple (rev_tuple ?tuple))" => "?tuple"
        ),
    ]
}

fn is_not_rev_tuple(
    var: &str,
) -> impl Fn(&mut EGraph<Operator, OperatorAnalyzer>, Id, &Subst) -> bool {
    let var = var.parse().unwrap();

    move |egraph, _id, subst| {
        egraph[subst[var]]
            .nodes
            .iter()
            .any(|node| matches!(node, Operator::ReverseTuple(_)))
    }
}

egg::test_fn! {
    fuse_maps,
    rules(),
    "(map (map input0 func0) func1)"
        => "(map input0 (apply func0 func1))"
}

egg::test_fn! {
    fuse_filters,
    rules(),
    "(filter (filter input0 func0) func1)"
        => "(filter input0 (and func0 func1))"
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
        => "(filter_map input0 (filter_opt func0 func1))"
}

egg::test_fn! {
    remove_redundant_arrange_by_keys,
    rules(),
    "(arrange_by_key (arrange_by_key (arrange_by_key input0)))"
        => "(arrange_by_key input0)"
}

egg::test_fn! {
    remove_redundant_arrange_by_selfs,
    rules(),
    "(arrange_by_self (arrange_by_self (arrange_by_self input0)))"
        => "(arrange_by_self input0)"
}

#[test]
fn join_map_and_tuples_saturate() {
    let expr: RecExpr<Operator> = "(join_map input0 input1 func0)".parse().unwrap();

    let runner: Runner<Operator, OperatorAnalyzer> = Runner::default()
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
    let expr: RecExpr<Operator> = "(join_filter input0 input1 func0)".parse().unwrap();

    let runner: Runner<Operator, OperatorAnalyzer> = Runner::default()
        .with_iter_limit(3)
        .with_expr(&expr)
        .run(&rules());

    assert!(
        matches!(runner.stop_reason, Some(StopReason::Saturated)),
        "stopped with reason {:?}",
        runner.stop_reason,
    );
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Input {
    id: usize,
}

impl Display for Input {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("input")?;
        Display::fmt(&self.id, f)
    }
}

impl FromStr for Input {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let string = string
            .strip_prefix("input")
            .ok_or_else(|| format!("expected 'input<int>', got '{}'", string))?;

        let id = string
            .parse::<usize>()
            .map_err(|err| format!("id must be an integer: {:?}", err))?;

        Ok(Self { id })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Output {
    id: usize,
}

impl Display for Output {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("output")?;
        Display::fmt(&self.id, f)
    }
}

impl FromStr for Output {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let string = string
            .strip_prefix("output")
            .ok_or_else(|| format!("expected 'output<int>', got '{}'", string))?;

        let id = string
            .parse::<usize>()
            .map_err(|err| format!("id must be an integer: {:?}", err))?;

        Ok(Self { id })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Func {
    id: usize,
}

impl Display for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("func")?;
        Display::fmt(&self.id, f)
    }
}

impl FromStr for Func {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let string = string
            .strip_prefix("func")
            .ok_or_else(|| format!("expected 'func<int>', got '{}'", string))?;

        let id = string
            .parse::<usize>()
            .map_err(|err| format!("id must be an integer: {:?}", err))?;

        Ok(Self { id })
    }
}
