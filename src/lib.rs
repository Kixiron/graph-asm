mod cost;
mod operator;

use std::collections::HashMap;

pub use cost::OperatorCost;
pub use operator::{Func, Index, Input, Operator, Output};

use egg::{rewrite, Analysis, Id, Language, Subst};
#[cfg(test)]
use egg::{Extractor, StopReason};
use velcro::vec;

type EGraph = egg::EGraph<Operator, OperatorAnalyzer>;
type Rewrite = egg::Rewrite<Operator, OperatorAnalyzer>;
type Pattern = egg::Pattern<Operator>;
#[cfg(test)]
type Runner = egg::Runner<Operator, OperatorAnalyzer>;
#[cfg(test)]
type RecExpr = egg::RecExpr<Operator>;

#[derive(Debug, Default)]
pub struct OperatorAnalyzer;

#[derive(Debug, Default)]
pub struct OperatorData {
    constant: Option<Constant>,
    ty: Option<Type>,
    decls: HashMap<Id, Id>,
}

impl Analysis<Operator> for OperatorAnalyzer {
    type Data = OperatorData;

    fn make(egraph: &EGraph, enode: &Operator) -> Self::Data {
        // FIXME: This doesn't propagate correctly
        let mut decls = HashMap::new();
        if let Operator::Let([var, val, _]) = *enode {
            decls.insert(var, val);
        }

        let ty = typecheck(egraph, enode);
        let constant = evaluate(egraph, enode);

        OperatorData {
            constant,
            ty,
            decls,
        }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        let did_change = to.decls != from.decls;
        to.decls.extend(from.decls);

        if to.constant.is_none() && from.constant.is_some() {
            to.constant = from.constant;
            true
        } else {
            did_change
        }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        if let Some(constant) = egraph[id].data.constant.clone() {
            let constant = constant.as_operator(egraph);
            let const_id = egraph.add(constant);

            egraph.union(id, const_id);
        }

        // Union variable usages with the variable's value
        let decls = egraph[id].data.decls.clone();
        for node in egraph[id].nodes.clone() {
            for &child in node.children() {
                for child_node in egraph[child].nodes.clone() {
                    let new_child = egraph.add(child_node);
                    egraph[new_child].data.decls.extend(decls.clone());
                    egraph.union(child, new_child);
                }
            }

            if let Operator::Var(var) = node {
                debug_assert!(egraph[var].nodes.iter().all(Operator::is_symbol));

                if let Some(&body) = egraph[var].data.decls.get(&var) {
                    egraph.union(id, body);
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    UInt,
    Unit,
    List(Box<Type>),
    Tuple(Vec<Type>),
    Option(Box<Type>),
    Lambda(Vec<Type>, Box<Type>),
    Collection(Box<Type>),
    Unknown,
}

impl Type {
    /// Returns `true` if the type is [`Int`].
    pub const fn is_int(&self) -> bool {
        matches!(self, Self::Int)
    }

    /// Returns `true` if the type is [`UInt`].
    pub const fn is_uint(&self) -> bool {
        matches!(self, Self::UInt)
    }

    /// Returns `true` if the type is [`Unknown`].
    pub const fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    /// Returns `true` if the type is [`Assume`].
    pub const fn is_assume(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

// FIXME: Actual type errors
fn typecheck(graph: &EGraph, enode: &Operator) -> Option<Type> {
    let x = |&id: &Id| {
        graph[id]
            .data
            .ty
            .as_ref()
            .filter(|ty| !ty.is_unknown() && !ty.is_assume())
            .cloned()
    };

    match enode {
        Operator::Input(_) => Some(Type::Collection(Box::new(Type::Unknown))),
        Operator::Output(_) => Some(Type::Collection(Box::new(Type::Unknown))),
        Operator::OpaqueFunc(_) => Some(Type::Lambda(vec![Type::Unknown], Box::new(Type::Unknown))),
        Operator::Empty => Some(Type::Collection(Box::new(Type::Unknown))),

        Operator::Add([lhs_id, rhs_id]) | Operator::Sub([lhs_id, rhs_id]) => {
            let (lhs, rhs) = (x(lhs_id)?, x(rhs_id)?);
            assert_eq!(lhs, rhs, "enodes {} and {} in {:?}", lhs_id, rhs_id, enode);

            if lhs.is_uint() {
                Some(Type::UInt)
            } else if lhs.is_int() {
                Some(Type::Int)
            } else {
                panic!(
                    "{:?} is not an acceptable add type for enodes {} and {}",
                    lhs, lhs_id, rhs_id,
                );
            }
        }

        Operator::Eq([lhs_id, rhs_id])
        | Operator::GreaterEq([lhs_id, rhs_id])
        | Operator::And([lhs_id, rhs_id])
        | Operator::Or([lhs_id, rhs_id]) => {
            let (lhs, rhs) = (x(lhs_id)?, x(rhs_id)?);
            assert_eq!(lhs, rhs, "enodes {} and {} in {:?}", lhs_id, rhs_id, enode);

            Some(Type::Bool)
        }

        Operator::If([cond_id, then_id, else_id]) => {
            // TODO: Replace with `assert_matches!(x(cond_id), Some(Type::Bool) | None)`
            if let Some(cond) = x(cond_id).filter(|cond| cond == &Type::Unknown) {
                assert_eq!(cond, Type::Bool, "if condition {} must be a bool", cond_id);
            }

            let (then, else_) = (x(then_id)?, x(else_id)?);
            assert_eq!(
                then, else_,
                "enodes {} and {} in {:?}",
                then_id, else_id, enode,
            );

            Some(then)
        }

        Operator::Unit => Some(Type::Unit),
        Operator::Int(_) => Some(Type::Int),
        Operator::UInt(_) => Some(Type::UInt),
        Operator::Bool(_) => Some(Type::Bool),

        Operator::Some(inner) => Some(Type::Option(Box::new(x(inner).unwrap_or(Type::Unknown)))),
        Operator::None => Some(Type::Option(Box::new(Type::Unknown))),

        Operator::Concat(collections) => {
            assert!(
                collections.iter().all(|coll| matches!(
                    x(coll),
                    Some(Type::Collection(_) | Type::Unknown) | None,
                )),
                "concatenation only works on streams {:?}",
                enode,
            );

            Some(Type::Collection(Box::new(
                collections
                    .iter()
                    .filter_map(x)
                    .find(|ty| ty != &Type::Unknown)
                    .unwrap_or(Type::Unknown),
            )))
        }

        // TODO
        &Operator::Var(var) => {
            assert!(
                graph[var].nodes.iter().all(Operator::is_symbol),
                "variables must be given symbols in {:?}",
                enode,
            );

            graph[var].data.decls.get(&var).and_then(x)
        }

        // TODO
        &Operator::Let([var, _, _]) => {
            assert!(
                graph[var].nodes.iter().all(Operator::is_symbol),
                "variables must be given symbols in {:?}",
                enode,
            );

            None
        }

        // TODO
        Operator::Symbol(_)
        | Operator::Map(_)
        | Operator::Filter(_)
        | Operator::FilterMap(_)
        | Operator::Join(_)
        | Operator::JoinMap(_)
        | Operator::JoinFilter(_)
        | Operator::Reduce(_)
        | Operator::ArrangeByKey(_)
        | Operator::ArrangeBySelf(_)
        | Operator::Consolidate(_)
        | Operator::AsCollection(_)
        | Operator::Apply(_)
        | Operator::AndThen(_)
        | Operator::FilterOption(_)
        | Operator::ReverseTuple(_)
        | Operator::Func(_)
        | Operator::List(_)
        | Operator::Tuple(_)
        | Operator::Case(_)
        | Operator::Index(_) => None,
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    Unit,
    Int(i64),
    UInt(u64),
    Bool(bool),
    // List(Vec<Constant>),
    // Tuple(Vec<Constant>),
    Option(Option<Box<Constant>>),
}

impl Constant {
    pub const fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(boolean) = *self {
            Some(boolean)
        } else {
            None
        }
    }

    pub fn add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(Constant::Int(lhs.saturating_add(rhs))),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(Constant::UInt(lhs.saturating_add(rhs))),

            (_, _) => None,
        }
    }

    pub fn sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(Constant::Int(lhs.saturating_sub(rhs))),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(Constant::UInt(lhs.saturating_sub(rhs))),

            (_, _) => None,
        }
    }

    pub fn eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(true),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs == rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs == rhs),
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs == rhs),

            (Self::Option(None), Self::Option(None)) => Some(true),
            (Self::Option(None), Self::Option(Some(_)))
            | (Self::Option(Some(_)), Self::Option(None)) => Some(false),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.eq(rhs),

            (_, _) => None,
        }
    }

    pub fn greater_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Some(true),
            (&Self::Int(lhs), &Self::Int(rhs)) => Some(lhs >= rhs),
            (&Self::UInt(lhs), &Self::UInt(rhs)) => Some(lhs >= rhs),
            (&Self::Bool(lhs), &Self::Bool(rhs)) => Some(lhs >= rhs),

            (Self::Option(None), Self::Option(Some(_))) => Some(false),
            (Self::Option(Some(_)), Self::Option(None)) => Some(true),
            (Self::Option(None), Self::Option(None)) => Some(true),
            (Self::Option(Some(lhs)), Self::Option(Some(rhs))) => lhs.greater_eq(rhs),

            (_, _) => None,
        }
    }

    pub fn as_operator(&self, egraph: &mut EGraph) -> Operator {
        match self {
            Constant::Unit => Operator::Unit,
            &Constant::Int(int) => Operator::Int(int),
            &Constant::UInt(uint) => Operator::UInt(uint),
            &Constant::Bool(boolean) => Operator::Bool(boolean),
            Constant::Option(None) => Operator::None,
            Constant::Option(Some(inner)) => {
                let inner = inner.as_operator(egraph);
                let inner = egraph.add(inner);

                Operator::Some(inner)
            }
        }
    }

    pub fn is_true(&self) -> Option<bool> {
        if let Self::Bool(boolean) = *self {
            Some(boolean)
        } else {
            None
        }
    }
}

fn evaluate(egraph: &EGraph, enode: &Operator) -> Option<Constant> {
    let x = |&id: &Id| egraph[id].data.constant.clone();

    match enode {
        // Evaluate what expressions we can
        Operator::And([lhs, rhs]) => Some(Constant::Bool(x(lhs)?.as_bool()? && x(rhs)?.as_bool()?)),
        Operator::Or([lhs, rhs]) => Some(Constant::Bool(x(lhs)?.as_bool()? || x(rhs)?.as_bool()?)),

        Operator::Add([lhs, rhs]) => x(lhs)?.add(&x(rhs)?),
        Operator::Sub([lhs, rhs]) => x(lhs)?.sub(&x(rhs)?),

        Operator::Eq([lhs, rhs]) => Some(Constant::Bool(x(lhs)?.eq(&x(rhs)?)?)),
        Operator::GreaterEq([lhs, rhs]) => Some(Constant::Bool(x(lhs)?.greater_eq(&x(rhs)?)?)),

        // TODO: This may actually get covered by the standard rewrite rules
        Operator::FilterOption([optional, filter]) => {
            if x(filter)?.is_true()? {
                x(optional)
            } else {
                Some(Constant::Option(None))
            }
        }

        // TODO: This may actually get covered by the standard rewrite rules
        Operator::If([cond, then, otherwise]) => {
            if x(cond)?.is_true()? {
                x(then)
            } else {
                x(otherwise)
            }
        }

        // Return constants directly
        Operator::Unit => Some(Constant::Unit),
        Operator::None => Some(Constant::Option(None)),
        // FIXME: Figure out how to get the inner value of a `Some` enode
        Operator::Some(inner) => x(inner).map(|inner| Constant::Option(Some(Box::new(inner)))),
        &Operator::Int(int) => Some(Constant::Int(int)),
        &Operator::UInt(uint) => Some(Constant::UInt(uint)),
        &Operator::Bool(boolean) => Some(Constant::Bool(boolean)),

        // TODO
        Operator::Index(_)
        | Operator::Case(_)
        | Operator::Apply(_)
        | Operator::AndThen(_)
        | Operator::ReverseTuple(_)
        | Operator::Func(_)
        | Operator::Let(_)
        | Operator::Var(_)
        | Operator::List(_)
        | Operator::Tuple(_) => None,

        // Symbols don't really mean anything, only
        // variable uses do
        Operator::Symbol(_) => None,

        // We can't statically evaluate dataflow operators
        // TODO: Or can we...
        Operator::Input(_)
        | Operator::Output(_)
        | Operator::OpaqueFunc(_)
        | Operator::Empty
        | Operator::Map(_)
        | Operator::Filter(_)
        | Operator::FilterMap(_)
        | Operator::Join(_)
        | Operator::JoinMap(_)
        | Operator::JoinFilter(_)
        | Operator::Reduce(_)
        | Operator::ArrangeByKey(_)
        | Operator::ArrangeBySelf(_)
        | Operator::Concat(_)
        | Operator::Consolidate(_)
        | Operator::AsCollection(_) => None,
    }
}

#[rustfmt::skip]
pub fn rules() -> Vec<Rewrite> {
    vec![
        ..join_laws(),

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

        ..logical_expressions(),

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

        ..empty_collections(),

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
        ..collapse_concatenation(),
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

#[rustfmt::skip]
fn logical_expressions() -> Vec<Rewrite> {
    vec![
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

fn is_not_rev_tuple(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let var = var.parse().unwrap();

    fn is_not_rev_tuple(id: Id, graph: &EGraph) -> bool {
        let mut nodes = graph[id].nodes.clone();
        while let Some(node) = nodes.pop() {
            match node {
                Operator::ReverseTuple(_) => return false,
                Operator::Apply([_, id]) | Operator::Func(id) => {
                    nodes.extend(graph[id].nodes.iter().cloned());
                }

                _ => {}
            }
        }

        true
    }

    move |graph, _id, subst| is_not_rev_tuple(subst[var], graph)
}

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
    "(== (+ 1 (- 1000 900)) (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1))))))))"
        => "false"
}

egg::test_fn! {
    merge_differently_named_functions,
    rules(),
    "(list (fun (+ #0 1)) (fun (+ #0 1)))"
        => "(list (fun (+ #0 1)) (fun (+ #0 1)))"
}

egg::test_fn! {
    adjacent_arrangements,
    rules(),
    "(arrange_by_key (as_collection (arrange_by_key ?x) (fun #0)))"
        => "(arrange_by_key ?x)"
}

egg::test_fn! {
    variable_inlining,
    rules(),
    "(let foo 10 (+ foo 10))"
        => "(let foo 10 20)"
}

#[test]
fn join_map_and_tuples_saturate() {
    let expr: RecExpr = "(join_map input0 input1 func0)".parse().unwrap();

    let runner: Runner = Runner::default()
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

    let runner: Runner = Runner::default()
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
    const FIBONACCI: &str =
        "(let FibonacciFor input0
            (let NeedsFibonacci
                (concat (var FibonacciFor)
                        (filter_map (var FibonacciFor)
                            (fun (if (>= #0 1)
                                     (some (- #0 1))
                                     none)))
                        (filter_map (var FibonacciFor)
                            (fun (if (>= #0 2)
                                     (some (- #0 2))
                                     none))))
                (let FibonacciLoop
                    (map
                        (join
                            (arrange_by_key
                                (map
                                    (join (arrange_by_self NeedsFibonacci)
                                          (arrange_by_key (map FibonacciLoop (fun (- #0 1)))))
                                    (fun (case #0 (tuple _ _) (tuple #3 #0)))))
                            (arrange_by_key
                                (map
                                    (join (arrange_by_self NeedsFibonacci)
                                          (arrange_by_key (map FibonacciLoop (fun (- #0 2)))))
                                    (fun (case #0 (tuple _ _) (tuple #3 #0))))))
                        (fun (case #0 (tuple _ _) (tuple #3 (+ #1 #0)))))
                    unit)))";

    let expr: RecExpr = FIBONACCI.parse().unwrap();
    let runner = Runner::default().with_expr(&expr).run(&rules());
    let (egraph, root) = (runner.egraph, runner.roots[0]);

    let mut extractor = Extractor::new(&egraph, OperatorCost);
    let (best_cost, best) = extractor.find_best(root);
    println!("{}: {}", best_cost, best.pretty(80));

    let mut ids: Vec<Id> = egraph.classes().map(|class| class.id).collect();
    ids.sort();

    for id in ids {
        let node = &egraph[id];
        let data = &node.data;

        let mut nodes = node.nodes.clone();
        nodes.sort();

        print!("{}: {:?}", id, nodes);
        if let Some(constant) = data.constant.as_ref() {
            print!(", constant: {:?}", constant);
        }
        if let Some(ty) = data.ty.as_ref() {
            print!(", type: {:?}", ty);
        }
        if !data.decls.is_empty() {
            print!(", decls: {:?}", data.decls);
        }
        println!();
    }
}
