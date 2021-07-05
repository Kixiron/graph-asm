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
        // rewrite!(
        //     "commutative-concatenation";
        //     "(concat ?stream1 ?stream2)"
        //         => "(concat ?stream2 ?stream1)"
        // ),

        rewrite!(
            "eliminate-empty-concat/1";
            "(concat)" => "empty"
        ),
        // TODO: Make a variadic rewrite for this
        rewrite!(
            "eliminate-empty-concat/2";
            "(concat empty empty)" => "empty"
        ),

        rewrite!(
            "collapse-concatenation";
            { FindNestedConcatenation }
                => { CollapseNestedConcatenation }
        ),

        // TODO: Remove empty streams from concats
    ]
}

struct FindNestedConcatenation;

impl Searcher<Operator, OperatorAnalyzer> for FindNestedConcatenation {
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

struct CollapseNestedConcatenation;

impl Applier<Operator, OperatorAnalyzer> for CollapseNestedConcatenation {
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

#[cfg(test)]
mod tests {
    use crate::rewrites::rules;

    egg::test_fn! {
        concatenation_collapses,
        rules(),
        "(concat ?a (concat ?b (concat ?c ?d ?e ?f ?g) ?h ?i ?j) ?k ?l ?m ?n ?o ?p)"
            => "(concat ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p)"
    }
}
