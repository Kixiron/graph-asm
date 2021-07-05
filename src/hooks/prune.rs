use crate::{operator::Operator, Runner};

pub fn prune(runner: &mut Runner) {
    let Runner {
        egraph: graph,
        iterations,
        ..
    } = runner;

    if iterations.len() % 5 == 0 {
        for class in graph.classes_mut() {
            // FIXME: Straight up incorrect, doesn't respect required nodes
            if class.nodes.iter().any(Operator::is_constant) {
                class.nodes.retain(Operator::is_constant);
                class.nodes.shrink_to_fit();
            }
        }
    }
}
