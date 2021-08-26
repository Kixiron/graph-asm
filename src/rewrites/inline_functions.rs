use crate::{operator::Operator, EGraph, OperatorAnalyzer, Rewrite};
use egg::{rewrite, Applier, Id, Language, Subst, Var};

// - for each match of `(apply (func ?body) ?args)`
//     - within `?body`
//         - replace each occurrence of `#0` (de bruijn indices
//           of level zero) with `?args`
//         - decrement all other de bruijn indices by one
//           (`#1` -> `#0`)
//     - replace the `(apply ?func ?args)` motif with the newly
//       rewritten `?body`

pub fn inline_functions() -> Rewrite {
    rewrite!(
        "inline-functions";
        "(apply (fun ?body) ?args)"
            => { Inliner::new("?body", "?args") }
    )
}

struct Inliner {
    body: Var,
    args: Var,
}

impl Inliner {
    #[track_caller]
    pub fn new(body: &str, args: &str) -> Self {
        Self {
            body: body.parse().unwrap(),
            args: args.parse().unwrap(),
        }
    }
}

impl Applier<Operator, OperatorAnalyzer> for Inliner {
    fn apply_one(&self, egraph: &mut EGraph, _eclass: Id, subst: &Subst) -> Vec<Id> {
        let (body, args) = (egraph.find(subst[self.body]), egraph.find(subst[self.args]));

        let mut rewritten = Vec::with_capacity(egraph[body].nodes.len());
        for mut node in egraph[body].nodes.clone() {
            node.update_children(|id| {
                for node in egraph[id].nodes.clone() {
                    if let Operator::Index(index) = node {
                        if index.is_zero() {
                            return args;
                        } else {
                            let index = egraph.add(Operator::Index(index.decrement()));
                            return egraph.find(index);
                        }
                    }
                }

                id
            });

            let node = egraph.add(node);
            rewritten.push(egraph.find(node));
        }

        rewritten
    }

    fn vars(&self) -> Vec<Var> {
        vec![self.body, self.args]
    }
}

#[cfg(test)]
mod tests {
    use crate::{rewrites::rules, RecExpr, Runner};
    use egg::StopReason;

    egg::test_fn! {
        function_inlining,
        rules(),
        "(apply (fun (add #0 #0)) 2)"
            => "4"
    }

    egg::test_fn! {
        function_inlining_respects_nesting,
        rules(),
        "(fun (apply (fun (add #0 #1)) 2))"
            => "(fun (add 2 #0))"
    }

    egg::test_fn! {
        nested_function_inlining,
        rules(),
        "(apply (fun (apply (fun (apply (fun (add #0 (add #1 #2))) 3)) 2)) 1)"
            => "6"
    }

    #[test]
    fn inlining_saturates() {
        // Equivalent to `(add 3 (add 2 1))` or `6`
        let expr: RecExpr = "(apply (fun (apply (fun (apply (fun (add #0 (add #1 #2))) 3)) 2)) 1)"
            .parse()
            .unwrap();

        let runner = Runner::default()
            .with_iter_limit(7)
            .with_expr(&expr)
            .run(&rules());

        let id = runner.egraph.find(*runner.roots.last().unwrap());
        let goals = vec!["6".parse().unwrap()];
        runner.egraph.check_goals(id, &goals);

        // FIXME: Use `assert_matches!()`
        assert!(
            matches!(runner.stop_reason, Some(StopReason::Saturated)),
            "stopped with reason {:?}",
            runner.stop_reason,
        );
    }
}
