use crate::{
    operator::Operator,
    rewrites::utils::{is_not_zero, is_numeric, is_zero},
    EGraph, OperatorAnalyzer, Rewrite, Type,
};
use egg::{rewrite, Applier, Id, Subst, Var};
use velcro::vec;

#[rustfmt::skip]
pub fn algebraic_laws() -> Vec<Rewrite> {
    vec![
        // Addition
        rewrite!(
            "commutative-add";
            "(add ?x ?y)" => "(add ?y ?x)"
        ),
        rewrite!(
            "associative-add";
            "(add ?x (add ?y ?z))" => "(add ?z (add ?x ?y))"
        ),
        ..rewrite!(
            "distributive-add";
            "(mul ?x (add ?y ?z))"
                <=> "(add (mul ?x ?y) (mul ?x ?z))"
        ),
        rewrite!(
            "add-identity";
            "(add ?x 0)" => "?x"
        ),
        rewrite!(
            "destructive-add";
            "(add ?x (neg ?x))" => "0"
        ),
        ..rewrite!(
            "add-self-into-mul";
            "(add ?x ?x)" <=> "(mul 2 ?x)"
        ),
        ..rewrite!(
            "negative-add";
            "(add ?x (neg ?y))" <=> "(sub ?x ?y)"
        ),

        // Subtraction
        ..rewrite!(
            "distributive-sub";
            "(mul ?x (sub ?y ?z))"
                <=> "(sub (mul ?x ?y) (mul ?x ?z))"
        ),
        rewrite!(
            "sub-identity";
            "(sub ?x 0)" => "?x"
        ),
        rewrite!(
            "destructive-sub";
            "(sub ?x ?x)" => "0"
        ),
        rewrite!(
            "sub-zero";
            "(sub 0 ?x)" => "(neg ?x)"
        ),

        // Multiplication
        rewrite!(
            "commutative-mul";
            "(mul ?x ?y)" => "(mul ?y ?x)"
        ),
        rewrite!(
            "associative-mul";
            "(mul ?x (mul ?y ?z))" => "(mul ?z (mul ?x ?y))"
        ),
        rewrite!(
            "mul-identity";
            "(mul ?x 1)" => "?x"
        ),
        rewrite!(
            "destructive-mul";
            "(mul ?x 0)" => "0"
        ),
        ..rewrite!(
            "negative-mul";
            "(mul ?x -1)" <=> "(neg ?x)"
        ),
        rewrite!(
            "multiplicative-inverse";
            "(mul ?x (div 1 ?x))" => "1"
        ),
        ..rewrite!(
            "neg-to-mul";
            "(neg ?x)" <=> "(mul ?x -1)"
        ),

        // Division
        ..rewrite!(
            "distributive-div-add";
            "(div (add ?x ?y) ?z)"
                <=> "(add (div ?x ?z) (div ?y ?z))"
        ),
        ..rewrite!(
            "distributive-div-sub";
            "(div (sub ?x ?y) ?z)"
                <=> "(sub (div ?x ?z) (div ?y ?z))"
        ),
        rewrite!(
            "div-identity";
            "(div ?x 1)" => "?x"
        ),
        rewrite!(
            "destructive-div";
            "(div ?x ?x)" => "1"
                // Divisive destruction doesn't apply to floats
                if is_numeric("?x")
                if is_not_zero("?x")
        ),
        ..rewrite!(
            "divisive-reciprocal";
            "(div ?x ?y)" <=> "(mul ?x (div 1 ?y))"
                if is_not_zero("?y")
        ),

        rewrite!(
            "redundant-neg";
            "(neg (neg ?x))" => "?x"
        ),
        ..rewrite!(
            "distributive-neg-add";
            "(neg (add ?x ?y))"
                <=> "(add (neg ?x) (neg ?y))"
        ),
        ..rewrite!(
            "distributive-neg-mul";
            "(neg (mul ?x ?y))"
                <=> "(mul ?x (neg ?y))"
        ),
        rewrite!(
            "eliminate-neg-mul";
            "(mul (neg ?x) (neg ?y))"
                => "(mul ?x ?y)"  
        ),

        // If `?x = (add ?x ?y)` then `?y` is zero
        rewrite!(
            "self-add-zero";
            "(add ?x ?y)" => { ImpliesValue::new("?x", "?y", ValKind::Zero) }
        ),
        // If `?x = (sub ?x ?y)` then `?y` is zero
        rewrite!(
            "self-sub-zero";
            "(sub ?x ?y)" => { ImpliesValue::new("?x", "?y", ValKind::Zero) }
        ),
        // If `?x = (mul ?x ?y)` then `?y` is one
        rewrite!(
            "self-mul-one";
            "(mul ?x ?y)" => { ImpliesValue::new("?x", "?y", ValKind::One) }
                if is_not_zero("?x")
                if is_not_zero("?y")
        ),
        // If `?x = (mul ?x 0)` then `?x` is zero
        rewrite!(
            "self-mul-zero";
            "(mul ?x ?y)" => { ImpliesValue::new("?x", "?y", ValKind::Zero) }
                if is_zero("?y")
        ),
        // If `?x = (div ?x ?y)` then `?y` is one
        rewrite!(
            "self-div-one";
            "(sub ?x ?y)" => { ImpliesValue::new("?x", "?y", ValKind::One) }
                if is_not_zero("?x")
                if is_not_zero("?y")
        ),
    ]
}

#[derive(Debug, Clone, Copy)]
pub enum ValKind {
    Zero,
    One,
}

#[derive(Debug)]
struct ImpliesValue {
    x: Var,
    y: Var,
    kind: ValKind,
}

impl ImpliesValue {
    #[track_caller]
    fn new(x: &str, y: &str, kind: ValKind) -> Self {
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
