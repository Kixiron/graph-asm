use crate::{operator::Operator, EGraph, OperatorAnalyzer, Type};
use egg::{Applier, Id, Subst, Var};

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

pub fn is_zero(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let var = var.parse().unwrap();

    move |graph, _id, subst| {
        graph[subst[var]]
            .nodes
            .iter()
            .all(|op| matches!(op, Operator::Int(0) | Operator::UInt(0)))
    }
}

pub fn is_not_zero(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let is_zero = is_zero(var);
    move |graph, id, subst| !is_zero(graph, id, subst)
}

#[derive(Debug, Clone, Copy)]
pub enum ValKind {
    Zero,
    One,
}

#[derive(Debug)]
pub struct ImpliesValue {
    x: Var,
    y: Var,
    kind: ValKind,
}

impl ImpliesValue {
    #[track_caller]
    pub fn new(x: &str, y: &str, kind: ValKind) -> Self {
        Self {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
            kind,
        }
    }
}

impl Applier<Operator, OperatorAnalyzer> for ImpliesValue {
    fn apply_one(&self, graph: &mut EGraph, eclass: Id, subst: &Subst) -> Vec<Id> {
        let eclass = graph.find(eclass);
        let (x, y) = (graph.find(subst[self.x]), graph.find(subst[self.y]));

        // TODO: This isn't ideal since inference can fail
        if let Some(ty) = graph[eclass]
            .data
            .ty
            .clone()
            .or_else(|| graph[x].data.ty.clone())
            .or_else(|| graph[y].data.ty.clone())
        {
            let value = match self.kind {
                ValKind::Zero => 0,
                ValKind::One => 1,
            };
            let value = match ty {
                Type::Int => Operator::Int(value),
                Type::UInt => Operator::UInt(value as u64),

                _ => return Vec::new(),
            };

            if eclass == x {
                let value = graph.add(value);
                graph.union(y, value);
            } else if eclass == y {
                let value = graph.add(value);
                graph.union(x, value);
            }
        }

        Vec::new()
    }

    fn vars(&self) -> Vec<Var> {
        vec![self.x, self.y]
    }
}
