use crate::operator::Operator;
use egg::{CostFunction, Id, Language};

#[derive(Debug, Default)]
pub struct OperatorCost;

impl OperatorCost {
    const INFINITY: f64 = f64::INFINITY;
    const ZERO: f64 = 0.0;
    const ONE: f64 = 1.0;
    const CONSTANT: f64 = 0.0;

    /// Operations that don't change the *volume* of data within a stream
    /// at all and just produce records at a 1:1 ratio
    const PASSTHROUGH: f64 = 1.0;

    /// Operations that cull the stream of values are less expensive
    /// than ones that create or are (in regards to the volume of data)
    /// passthrough
    const CULLING: f64 = 0.8;

    /// Arrangements are relatively expensive
    const ARRANGEMENT: f64 = 4.0;

    /// Joins are treated as more expensive than a "normal" operator since
    /// they operate over really large amounts of data
    const JOIN: f64 = 2.5;
}

impl CostFunction<Operator> for OperatorCost {
    type Cost = f64;

    fn cost<C>(&mut self, enode: &Operator, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let operator_cost = match enode {
            // Inputs and outputs have a cost of one
            Operator::Input(_) | Operator::Output(_) => return Self::ONE,

            // Empty collections have zero cost whatsoever
            Operator::Empty => return Self::ZERO,

            // We never want to extract consolidation so we give it
            // an infinite cost
            Operator::Consolidate(_) => return Self::INFINITY,

            // Mapping doesn't change the volume of data going through it
            Operator::Map(_) => Self::PASSTHROUGH,

            // Filtering culls the stream
            Operator::Filter(_) | Operator::FilterMap(_) => Self::CULLING,

            // We optimistically assume that reductions cull the stream,
            // in the future we should decompose reduce into its core
            // parts
            Operator::Reduce(_) => Self::CULLING,

            // Joins are really expensive, we pessimistically estimate that the
            // number of records produced will be the product of both inputs
            &Operator::Join([lhs, rhs]) => return Self::JOIN + (costs(lhs) * costs(rhs)),

            // Joins are really expensive, we pessimistically estimate that the
            // number of records produced will be the product of both inputs
            //
            // Note: We *could* give `JoinFilter` a reduced penalty but I don't
            //       know how much it'll matter
            &Operator::JoinMap([lhs, rhs, func]) | &Operator::JoinFilter([lhs, rhs, func]) => {
                return Self::JOIN + costs(func) + (costs(lhs) * costs(rhs));
            }

            // Arrangements are arrangements
            Operator::ArrangeByKey(_) => Self::ARRANGEMENT,

            // We give arrange_by_self an accurate cost picture, it consists of
            // an arrange_by_key and a map so we represent that accurately
            Operator::ArrangeBySelf(_) => Self::ARRANGEMENT + Self::PASSTHROUGH,

            // Turning an arrangement into a collection is a passthrough cost
            Operator::AsCollection(_) => Self::PASSTHROUGH,

            // Concatenation creates an output stream with aggregated
            // cost of all input streams
            Operator::Concat(_) => enode.fold(Self::ONE, |sum, id| sum + costs(id)),

            // We use a basic ast depth based method to estimate the cost
            // of imperative functions running within operators
            Operator::OpaqueFunc(_)
            | Operator::Apply(_)
            | Operator::AndThen(_)
            | Operator::FilterOption(_)
            | Operator::ReverseTuple(_)
            | Operator::If(_)
            | Operator::And(_)
            | Operator::Or(_)
            | Operator::Case(_)
            | Operator::Add(_)
            | Operator::Sub(_)
            | Operator::Eq(_)
            | Operator::GreaterEq(_) => Self::ONE,

            // We penalize variables *slightly* compared to constant
            // values just to encourage the usage of constants
            Operator::Var(_) | Operator::Index(_) => Self::CONSTANT + 0.1,

            // Constant values shouldn't be penalized in any way
            Operator::Func(_)
            | Operator::Let(_)
            | Operator::Some(_)
            | Operator::None
            | Operator::List(_)
            | Operator::Tuple(_)
            | Operator::Unit
            | Operator::Int(_)
            | Operator::UInt(_)
            | Operator::Bool(_)
            | Operator::Symbol(_) => Self::CONSTANT,
        };

        enode.fold(operator_cost, |sum, id| sum + costs(id))
    }
}
