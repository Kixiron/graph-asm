use crate::{operator::Operator, EGraph};
use egg::{Id, Language};
use im::{HashMap, HashSet};

pub fn variable_propagation(graph: &mut EGraph, roots: &[Id]) {
    let mut to_union = Vec::new();
    let mut stack: Vec<_> = roots
        .iter()
        .map(|&id| (id, HashMap::new(), HashSet::new()))
        .collect();

    while let Some((id, variables, visited)) = stack.pop() {
        let class = &graph[id];

        for node in class.nodes.iter() {
            propagate(
                variables.clone(),
                id,
                node,
                &mut to_union,
                visited.clone(),
                &mut stack,
            );
        }
    }

    for (id, new_id) in to_union {
        graph.union(id, new_id);
    }
}

fn propagate(
    old_variables: HashMap<Id, Id>,
    node_id: Id,
    node: &Operator,
    to_union: &mut Vec<(Id, Id)>,
    visited: HashSet<Id>,
    stack: &mut Vec<(Id, HashMap<Id, Id>, HashSet<Id>)>,
) {
    let mut variables = old_variables.clone();

    let mut ignored = None;
    if let Operator::Let([var, val, _]) = *node {
        variables = variables.update(var, val);
        ignored = Some(val);
    } else if let Operator::Var(var) = node {
        if let Some(&value) = variables.get(var) {
            to_union.push((node_id, value));
        }
    }

    stack.reserve(node.children().len());
    for &child in node.children() {
        if !visited.contains(&child) {
            let vars = if Some(child) == ignored {
                old_variables.clone()
            } else {
                variables.clone()
            };

            stack.push((child, vars, visited.update(child)));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{hooks, rewrites::rules, Runner};

    egg::test_fn! {
        variable_inlining,
        rules(),
        runner = Runner::default().with_hook(|runner| {
            hooks::variable_propagation(&mut runner.egraph, &runner.roots);
            Ok(())
        }),
        "(let foo 10 (add (var foo) 10))"
            => "(let foo 10 20)"
    }

    egg::test_fn! {
        #[should_panic(expected = "Could not prove goal 0")]
        variable_inlining_doesnt_leak,
        rules(),
        runner = Runner::default().with_hook(|runner| {
            hooks::variable_propagation(&mut runner.egraph, &runner.roots);
            Ok(())
        }),
        "(add (var foo) (let foo 10 (add (var foo) 10)))"
            => "(add 20 (let foo 10 20))"
    }
}
