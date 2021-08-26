use crate::{operator::Operator, EGraph, Type};
use egg::{Id, Symbol};

// TODO: This is actually critical since it fuels the implication-based
//       binary operation simplifiers, meaning that if typechecking isn't
//       good we can easily spiral into an infinite egraph
pub fn typecheck(graph: &mut EGraph, _roots: &[Id]) {
    let mut types = Vec::new();

    for class in graph.classes() {
        for node in class.nodes.iter() {
            match *node {
                Operator::Add([lhs_id, rhs_id])
                | Operator::Sub([lhs_id, rhs_id])
                | Operator::Mul([lhs_id, rhs_id])
                | Operator::Div([lhs_id, rhs_id])
                | Operator::Shl([lhs_id, rhs_id])
                | Operator::Shr([lhs_id, rhs_id])
                | Operator::BitXor([lhs_id, rhs_id])
                | Operator::BitOr([lhs_id, rhs_id])
                | Operator::BitAnd([lhs_id, rhs_id])
                | Operator::Eq([lhs_id, rhs_id])
                | Operator::NotEq([lhs_id, rhs_id])
                | Operator::GreaterEq([lhs_id, rhs_id])
                | Operator::Greater([lhs_id, rhs_id])
                | Operator::LessEq([lhs_id, rhs_id])
                | Operator::Less([lhs_id, rhs_id]) => {
                    let (lhs, rhs) = (&graph[lhs_id], &graph[rhs_id]);

                    if matches!(&lhs.data.ty, Some(ty) if !ty.is_unknown())
                        && matches!(rhs.data.ty, Some(Type::Unknown) | None)
                    {
                        types.push((rhs_id, lhs.data.ty.clone().unwrap()));
                    } else if matches!(lhs.data.ty, Some(Type::Unknown) | None)
                        && matches!(&rhs.data.ty, Some(ty) if !ty.is_unknown())
                    {
                        types.push((lhs_id, rhs.data.ty.clone().unwrap()));
                    }
                }

                Operator::And([lhs_id, rhs_id]) | Operator::Or([lhs_id, rhs_id]) => {
                    if !matches!(
                        graph[lhs_id].data.ty,
                        Some(Type::Bool | Type::Unknown) | None
                    ) {
                        panic!(
                            "incorrect types, {} of {} is a {:?}",
                            lhs_id, class.id, graph[lhs_id].data.ty,
                        );
                    } else if !matches!(
                        graph[rhs_id].data.ty,
                        Some(Type::Bool | Type::Unknown) | None
                    ) {
                        panic!(
                            "incorrect types, {} of {} is a {:?}",
                            rhs_id, class.id, graph[rhs_id].data.ty,
                        );
                    }

                    types.push((lhs_id, Type::Bool));
                    types.push((rhs_id, Type::Bool));
                }

                _ => {}
            }
        }
    }

    for (node, ty) in types {
        graph[node].data.ty = Some(ty.clone());

        // Add nodes with explicit types into the egraph in order to
        // keep saturation from being achieved while there's still
        // possible type-based optimizations to be made
        let type_name = match ty {
            Type::Bool => "bool",
            Type::Int => "i64",
            Type::UInt => "u64",
            Type::Unit => "unit",
            Type::List(_)
            | Type::Tuple(_)
            | Type::Option(_)
            | Type::Lambda(_, _)
            | Type::Collection(_)
            | Type::Unknown => continue,
        };
        let type_name = graph.add(Operator::Symbol(Symbol::from(type_name)));
        let typed = graph.add(Operator::Other(
            Symbol::from("typed"),
            vec![type_name, node],
        ));

        graph.union(node, typed);
    }
}
