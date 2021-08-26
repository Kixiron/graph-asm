use crate::{operator::Operator, EGraph};
use egg::Id;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    UInt,
    Unit,
    List(Box<Self>),
    Tuple(Vec<Self>),
    Option(Box<Self>),
    Lambda(Vec<Self>, Box<Self>),
    Collection(Box<Self>),
    Unknown,
}

impl Type {
    /// Returns `true` if the type is [`Int`].
    pub const fn is_int(&self) -> bool {
        matches!(self, Self::Int)
    }

    /// Returns `true` if the type is [`UInt`].
    pub const fn is_uint(&self) -> bool {
        matches!(self, Self::UInt)
    }

    /// Returns `true` if the type is [`Unknown`].
    pub const fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    pub const fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub const fn is_numeric(&self) -> bool {
        matches!(self, Self::UInt | Self::Int)
    }

    // FIXME: Actual type errors
    pub fn typecheck(graph: &EGraph, enode: &Operator) -> Option<Self> {
        let x = |&id: &Id| {
            graph[id]
                .data
                .ty
                .as_ref()
                .filter(|ty| !ty.is_unknown())
                .cloned()
        };

        match enode {
            Operator::Unit => Some(Self::Unit),
            Operator::Int(_) => Some(Self::Int),
            Operator::UInt(_) => Some(Self::UInt),
            Operator::Bool(_) => Some(Self::Bool),

            Operator::Tuple(elements) => {
                let types = elements
                    .iter()
                    .map(|elem| x(elem).unwrap_or(Self::Unknown))
                    .collect();

                Some(Self::Tuple(types))
            }

            Operator::List(elements) => {
                let mut ty = Self::Unknown;
                for elem in elements {
                    if let Some(elem_ty) = x(elem) {
                        if ty.is_unknown() {
                            ty = elem_ty;
                        } else if !elem_ty.is_unknown() {
                            assert_eq!(
                                elem_ty, ty,
                                "{:?} and {:?} (enode {}) in {:?}, lists have a single type",
                                ty, elem_ty, elem, enode,
                            );
                        }
                    }
                }

                Some(Self::List(Box::new(ty)))
            }

            Operator::Some(inner) => {
                Some(Self::Option(Box::new(x(inner).unwrap_or(Self::Unknown))))
            }
            Operator::None => Some(Self::Option(Box::new(Self::Unknown))),

            Operator::Input(_) | Operator::Output(_) => {
                Some(Self::Collection(Box::new(Self::Unknown)))
            }
            Operator::OpaqueFunc(_) => {
                Some(Self::Lambda(vec![Self::Unknown], Box::new(Self::Unknown)))
            }
            Operator::Empty => Some(Self::Collection(Box::new(Self::Unknown))),

            Operator::Add([lhs_id, rhs_id])
            | Operator::Sub([lhs_id, rhs_id])
            | Operator::Mul([lhs_id, rhs_id])
            | Operator::Div([lhs_id, rhs_id])
            | Operator::Shl([lhs_id, rhs_id])
            | Operator::Shr([lhs_id, rhs_id])
            | Operator::BitXor([lhs_id, rhs_id])
            | Operator::BitOr([lhs_id, rhs_id])
            | Operator::BitAnd([lhs_id, rhs_id]) => {
                let (lhs, rhs) = (x(lhs_id)?, x(rhs_id)?);
                assert_eq!(lhs, rhs, "enodes {} and {} in {:?}", lhs_id, rhs_id, enode);

                if lhs.is_uint() {
                    Some(Self::UInt)
                } else if lhs.is_int() {
                    Some(Self::Int)
                } else {
                    panic!(
                        "{:?} is not an acceptable add type for enodes {} and {}",
                        lhs, lhs_id, rhs_id,
                    );
                }
            }

            Operator::Neg(id) => {
                let val = x(id)?;
                assert!(val.is_int() || val.is_unknown());

                Some(val)
            }

            Operator::Not(id) => {
                let val = x(id)?;
                assert!(val.is_numeric() || val.is_bool() || val.is_unknown());

                Some(val)
            }

            Operator::Eq([lhs_id, rhs_id])
            | Operator::NotEq([lhs_id, rhs_id])
            | Operator::GreaterEq([lhs_id, rhs_id])
            | Operator::Greater([lhs_id, rhs_id])
            | Operator::LessEq([lhs_id, rhs_id])
            | Operator::Less([lhs_id, rhs_id])
            | Operator::And([lhs_id, rhs_id])
            | Operator::Or([lhs_id, rhs_id]) => {
                if let (Some(lhs), Some(rhs)) = (x(lhs_id), x(rhs_id)) {
                    if !lhs.is_unknown() && !rhs.is_unknown() {
                        assert_eq!(lhs, rhs, "enodes {} and {} in {:?}", lhs_id, rhs_id, enode);
                    }
                }

                Some(Self::Bool)
            }

            Operator::If([cond_id, then_id, else_id]) => {
                // TODO: Replace with `assert_matches!(x(cond_id), Some(Type::Bool) | None)`
                if let Some(cond) = x(cond_id).filter(|cond| cond == &Self::Unknown) {
                    assert_eq!(cond, Self::Bool, "if condition {} must be a bool", cond_id);
                }

                let (then, else_) = (x(then_id)?, x(else_id)?);
                assert_eq!(
                    then, else_,
                    "enodes {} and {} in {:?}",
                    then_id, else_id, enode,
                );

                Some(then)
            }

            Operator::Concat(collections) => {
                assert!(
                    collections.iter().all(|coll| matches!(
                        x(coll),
                        Some(Self::Collection(_) | Self::Unknown) | None,
                    )),
                    "concatenation only works on streams {:?}",
                    enode,
                );

                Some(Self::Collection(Box::new(
                    collections
                        .iter()
                        .filter_map(x)
                        .find(|ty| ty != &Self::Unknown)
                        .unwrap_or(Self::Unknown),
                )))
            }

            // TODO
            &Operator::Var(var) => {
                assert!(
                    graph[var].nodes.iter().all(Operator::is_symbol),
                    "variables must be given symbols in {:?}",
                    enode,
                );

                // graph[var].data.decls.get(&var).and_then(x)
                None
            }

            // TODO
            &Operator::Let([var, _, _]) => {
                assert!(
                    graph[var].nodes.iter().all(Operator::is_symbol),
                    "variables must be given symbols in {:?}",
                    enode,
                );

                None
            }

            Operator::Other(name, args) => match name.as_str() {
                // Untupling/tuple access
                "untuple" => {
                    // FIXME: `assert_matches!()`
                    assert!(matches!(
                        x(&args[0]),
                        Some(Type::Int | Type::UInt | Type::Unknown),
                    ));

                    let tuple = x(&args[1])?;

                    if let Type::Tuple(mut tuple) = tuple {
                        let index = &graph[args[0]].nodes[0];
                        let index = index
                            .as_uint()
                            .or_else(|| index.as_int().map(|int| int as u64))
                            .unwrap() as usize;

                        Some(tuple.remove(index))
                    } else {
                        // FIXME: Sometimes it's not a tuple?
                        None
                    }
                }

                // Explicitly typed nodes
                "typed" => {
                    let type_name = graph[args[0]]
                        .nodes
                        .iter()
                        .find_map(|node| node.as_symbol())
                        .unwrap();

                    let ty = match type_name.as_str() {
                        "bool" => Type::Bool,
                        "i64" => Type::Int,
                        "u64" => Type::UInt,
                        "unit" => Type::Unit,

                        // Unknown type name
                        _ => return None,
                    };

                    Some(ty)
                }

                _ => None,
            },

            // TODO
            Operator::Symbol(_)
            | Operator::Map(_)
            | Operator::Filter(_)
            | Operator::FilterMap(_)
            | Operator::Join(_)
            | Operator::JoinMap(_)
            | Operator::JoinFilter(_)
            | Operator::Reduce(_)
            | Operator::ArrangeByKey(_)
            | Operator::ArrangeBySelf(_)
            | Operator::Consolidate(_)
            | Operator::AsCollection(_)
            | Operator::Apply(_)
            | Operator::AndThen(_)
            | Operator::FilterOption(_)
            | Operator::ReverseTuple(_)
            | Operator::Func(_)
            | Operator::Index(_) => None,
        }
    }
}
