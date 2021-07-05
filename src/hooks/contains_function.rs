use crate::EGraph;
use std::collections::HashSet;

pub fn contains_function_call(graph: &mut EGraph) {
    let mut function_calls = HashSet::new();
    for class in graph.classes() {
        for node in class.nodes.iter() {
            if node.is_func() {
                function_calls.insert(class.id);
            }
        }
    }

    graph
        .analysis
        .contains_non_opaque_function_call
        .extend(function_calls);
}
