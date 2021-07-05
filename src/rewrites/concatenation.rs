use crate::{operator::Operator, EGraph, OperatorAnalyzer, Rewrite};
use egg::{rewrite, Applier, Id, Language, SearchMatches, Searcher, Subst, Var};
use velcro::vec;

#[rustfmt::skip]
pub fn concatenation() -> Vec<Rewrite> {
    vec![
        rewrite!(
            "eliminate-redundant-concat";
            "(concat ?stream)" => "?stream"
        ),

        rewrite!(
            "collapse-concatenation";
            { FindNestedConcat }
                => { CollapseNestedConcat }
        ),

        rewrite!(
            "eliminate-empty-concat";
            { FindConcatWithEmpty }
                => { RemoveEmptyFromConcat }
        ),

        rewrite!(
            "eliminate-uninhabited-concat";
            "(concat)" => "empty"
        ),
    ]
}

struct FindNestedConcat;

impl Searcher<Operator, OperatorAnalyzer> for FindNestedConcat {
    fn search_eclass(&self, graph: &EGraph, eclass: Id) -> Option<SearchMatches> {
        if graph[eclass]
            .nodes
            .iter()
            .filter(|node| node.is_concat())
            .flat_map(Language::children)
            .copied()
            .any(|child| graph[child].nodes.iter().any(Operator::is_concat))
        {
            Some(SearchMatches {
                eclass,
                substs: vec![Subst::with_capacity(1)],
            })
        } else {
            None
        }
    }

    fn vars(&self) -> Vec<Var> {
        Vec::new()
    }
}

struct CollapseNestedConcat;

impl Applier<Operator, OperatorAnalyzer> for CollapseNestedConcat {
    fn apply_one(&self, graph: &mut EGraph, eclass: Id, _subst: &Subst) -> Vec<Id> {
        let mut flattened_nodes = Vec::with_capacity(graph[eclass].nodes.len());
        for node in graph[eclass].nodes.clone() {
            if let Operator::Concat(streams) = node {
                for (idx, &id) in streams.iter().enumerate() {
                    for operator in graph[id].nodes.clone() {
                        let mut flattened_streams = Vec::with_capacity(streams.len());

                        for (idx2, &id2) in streams.iter().enumerate() {
                            if idx == idx2 {
                                if let Operator::Concat(streams) = &operator {
                                    flattened_streams
                                        .extend(streams.iter().map(|&id| graph.find(id)));
                                } else {
                                    flattened_streams.push(graph.find(id));
                                }
                            } else {
                                flattened_streams.push(graph.find(id2));
                            }
                        }

                        let node = graph.add(Operator::Concat(flattened_streams));
                        flattened_nodes.push(graph.find(node));
                    }
                }
            }
        }

        flattened_nodes.sort();
        flattened_nodes.dedup();
        flattened_nodes
    }
}

struct FindConcatWithEmpty;

impl Searcher<Operator, OperatorAnalyzer> for FindConcatWithEmpty {
    fn search_eclass(&self, graph: &EGraph, eclass: Id) -> Option<SearchMatches> {
        if graph[eclass]
            .nodes
            .iter()
            .filter(|node| node.is_concat())
            .flat_map(Language::children)
            .copied()
            .any(|child| graph[child].nodes.iter().any(Operator::is_empty))
        {
            Some(SearchMatches {
                eclass,
                substs: vec![Subst::with_capacity(1)],
            })
        } else {
            None
        }
    }

    fn vars(&self) -> Vec<Var> {
        Vec::new()
    }
}

struct RemoveEmptyFromConcat;

impl Applier<Operator, OperatorAnalyzer> for RemoveEmptyFromConcat {
    fn apply_one(&self, graph: &mut EGraph, eclass: Id, _subst: &Subst) -> Vec<Id> {
        let mut cleaned_nodes = Vec::with_capacity(graph[eclass].nodes.len());
        for node in graph[eclass].nodes.clone() {
            if let Operator::Concat(streams) = node {
                let streams = streams
                    .into_iter()
                    .filter(|&id| !graph[id].nodes.iter().any(Operator::is_empty))
                    .map(|id| graph.find(id))
                    .collect();

                let node = graph.add(Operator::Concat(streams));
                cleaned_nodes.push(graph.find(node));
            }
        }

        cleaned_nodes.sort();
        cleaned_nodes.dedup();
        cleaned_nodes
    }
}

#[cfg(test)]
mod tests {
    use crate::rewrites::rules;

    egg::test_fn! {
        concatenation_collapses,
        rules(),
        "(concat ?a (concat ?b (concat ?c ?d ?e ?f ?g) ?h ?i ?j) ?k ?l ?m ?n ?o ?p)"
            => "(concat ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p)"
    }

    egg::test_fn! {
        empty_streams_are_stripped_from_concat,
        rules(),
        "(concat empty ?a ?b ?c empty empty empty)"
            => "(concat ?a ?b ?c)"
    }

    egg::test_fn! {
        nested_empty_streams_are_stripped_from_concat,
        rules(),
        "(concat empty ?a ?b ?c (concat empty ?d ?e ?f ?g) empty (concat))"
            => "(concat ?a ?b ?c ?d ?e ?f ?g)"
    }

    egg::test_fn! {
        empty_concat_collapses,
        rules(),
        "(concat)" => "empty"
    }
}
