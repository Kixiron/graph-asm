use crate::{operator::Operator, EGraph};
use egg::{Id, Subst};

pub fn is_not_rev_tuple(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
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

pub fn is_numeric(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let var = var.parse().unwrap();

    move |graph, _id, subst| graph[subst[var]].nodes.iter().all(Operator::is_numeric)
}
