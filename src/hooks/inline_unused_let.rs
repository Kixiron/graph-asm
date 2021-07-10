use crate::{operator::Operator, EGraph};
use egg::{Id, Language};
use im::HashSet;

pub fn inline_unused_let(graph: &mut EGraph, roots: &[Id]) {
    let mut to_union = Vec::new();

    for &root in roots {
        let class = &graph[root];

        for node in class.nodes.iter() {
            find_used_vars(
                graph,
                root,
                node,
                HashSet::new(),
                HashSet::new(),
                &mut to_union,
            );
        }
    }

    for (id, new_id) in to_union {
        graph.union(id, new_id);
    }
}

// FIXME: Make this stack-based/iterative
fn find_used_vars(
    graph: &EGraph,
    node_id: Id,
    node: &Operator,
    mut used_variables: HashSet<Id>,
    visited: HashSet<Id>,
    to_union: &mut Vec<(Id, Id)>,
) -> HashSet<Id> {
    if let Operator::Var(var) = *node {
        used_variables.insert(var);
    }

    let mut output_used_variables = HashSet::new();
    for &child in node.children() {
        if !visited.contains(&child) {
            for node in graph[child].nodes.iter() {
                output_used_variables.extend(find_used_vars(
                    graph,
                    child,
                    &node,
                    used_variables.clone(),
                    visited.update(child),
                    to_union,
                ));
            }
        }
    }

    if let Operator::Let([var, _, body]) = *node {
        if !output_used_variables.contains(&var) {
            to_union.push((node_id, body));
        }

        output_used_variables.remove(&var);
    }

    output_used_variables
}

#[cfg(test)]
mod tests {
    use crate::{hooks, rewrites::rules, Runner};

    egg::test_fn! {
        inline_unused_let,
        rules(),
        runner = Runner::default().with_hook(|runner| {
            hooks::inline_unused_let(&mut runner.egraph, &runner.roots);
            Ok(())
        }),
        "(let foo 10 20)" => "20"
    }

    egg::test_fn! {
        variable_inlining_binding_gets_inlined,
        rules(),
        runner = Runner::default().with_hook(|runner| {
            hooks::variable_propagation(&mut runner.egraph, &runner.roots);
            hooks::inline_unused_let(&mut runner.egraph, &runner.roots);
            Ok(())
        }),
        "(let foo 10 (add (var foo) 10))"
            => "20"
    }

    egg::test_fn! {
        inline_unused_let_doesnt_leak,
        rules(),
        runner = Runner::default().with_hook(|runner| {
            hooks::inline_unused_let(&mut runner.egraph, &runner.roots);
            Ok(())
        }),
        "(let foo ?x (add (var foo) (let foo 10 20)))"
            => "(let foo ?x (add (var foo) 20))"
    }

    egg::test_fn! {
        inline_unused_let_doesnt_leak_but_inlining_still_happens,
        rules(),
        runner = Runner::default().with_hook(|runner| {
            hooks::inline_unused_let(&mut runner.egraph, &runner.roots);
            Ok(())
        }),
        "(let foo ?x (add (var foo) (let foo 10 20)))"
            => "(add ?x 20)"
    }

    egg::test_fn! {
        variable_inlining_and_propagation,
        rules(),
        runner = Runner::default()
            .with_hook(|runner| {
                hooks::variable_propagation(&mut runner.egraph, &runner.roots);
                Ok(())
            }).with_hook(|runner| {
                hooks::inline_unused_let(&mut runner.egraph, &runner.roots);
                Ok(())
            }),
        "(let bar 100 (add (var bar) (let foo 10 (add (var foo) 10))))"
            => "120"
    }
}
