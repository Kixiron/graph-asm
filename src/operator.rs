use egg::{Id, Symbol};
use std::{
    fmt::{self, Display, Formatter, Write},
    str::FromStr,
};

egg::define_language! {
    pub enum Operator {
        // An input operator
        Input(Input),
        // An output operator
        Output(Output),

        // An opaque function
        OpaqueFunc(Func),

        // An empty collection with no values
        "empty" = Empty,

        "map" = Map([Id; 2]),
        "filter" = Filter([Id; 2]),
        "filter_map" = FilterMap([Id; 2]),

        // `(join ?arr1 ?arr2)`
        "join" = Join([Id; 2]),

        // `(join_map ?arr1 ?arr2 ?map)` where `?map`
        // is a post-join map function
        "join_map" = JoinMap([Id; 3]),

        // `(join_filter ?arr1 ?arr2 ?filter)` where `?filter`
        // is a post-join filter function
        "join_filter" = JoinFilter([Id; 3]),

        // `(reduce ?arr ?reduce)` where `?reduce` is the
        // reduction function
        "reduce" = Reduce([Id; 2]),

        "arrange_by_key" = ArrangeByKey(Id),
        "arrange_by_self" = ArrangeBySelf(Id),

        "concat" = Concat(Vec<Id>),

        "consolidate" = Consolidate(Id),
        "as_collection" = AsCollection([Id; 2]),

        // `(apply ?func1 ?value)`
        "apply" = Apply([Id; 2]),

        // An `.and_then()` combinator applied to `Option`s
        "and_then" = AndThen([Id; 2]),

        // `(filter_opt ?option ?predicate)`
        // A version of `Option::filter()` that returns `None`
        // when the predicate is false and the given value
        // when true
        "filter_opt" = FilterOption([Id; 2]),

        // `(rev_tuple ?tuple)`
        // Reverses the order of values in a tuple,
        // turning `(?x, ?y)` into `(?y, ?x)`
        "rev_tuple" = ReverseTuple(Id),

        // `(if ?cond ?then ?else)`
        "if" = If([Id; 3]),

        // `(fun ?body)`
        "fun" = Func(Id),

        // TODO: Remove `let` and `var` in favor of a CPS form
        // `(let ?var ?value ?in)`
        "let" = Let([Id; 3]),
        // `(var ?name)`
        "var" = Var(Id),

        "some" = Some(Id),
        "none" = None,

        // `(list ?elems...)`
        "list" = List(Vec<Id>),
        // `(tuple ?elems...)`
        "tuple" = Tuple(Box<[Id]>),

        // `(case ?var ?pattern ?in)`
        "case" = Case([Id; 3]),

        "unit" = Unit,

        // `(and ?cond1 ?cond2)`
        "and" = And([Id; 2]),
        // `(or ?cond1 ?cond2)`
        "or" = Or([Id; 2]),
        // `(not ?x)`
        "not" = Not(Id),

        "add" = Add([Id; 2]),
        "sub" = Sub([Id; 2]),
        "mul" = Mul([Id; 2]),
        "div" = Div([Id; 2]),
        "neg" = Neg(Id),

        "eq" = Eq([Id; 2]),
        "neq" = NotEq([Id; 2]),
        ">=" = GreaterEq([Id; 2]),
        ">" = Greater([Id; 2]),
        "<=" = LessEq([Id; 2]),
        "<" = Less([Id; 2]),

        // A [De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index)
        Index(Index),

        Int(i64),
        UInt(u64),
        Bool(bool),
        Symbol(Symbol),
    }
}

impl Operator {
    pub const fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }

    pub const fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol(_))
    }

    pub const fn as_symbol(&self) -> Option<Symbol> {
        if let Self::Symbol(symbol) = *self {
            Some(symbol)
        } else {
            None
        }
    }

    pub const fn is_numeric(&self) -> bool {
        matches!(self, Self::Int(_) | Self::UInt(_))
    }

    pub const fn is_collection(&self) -> bool {
        matches!(
            self,
            Self::Input(_)
                | Self::Empty
                | Self::Map(_)
                | Self::Filter(_)
                | Self::FilterMap(_)
                | Self::Join(_)
                | Self::JoinMap(_)
                | Self::JoinFilter(_)
                // FIXME: Once we decompose reduce `.reduce_abelian()` is arranged
                | Self::Reduce(_)
                | Self::Concat(_)
                | Self::Consolidate(_)
                | Self::AsCollection(_)
        )
    }

    pub const fn is_arrangement(&self) -> bool {
        matches!(self, Self::ArrangeByKey(_) | Self::ArrangeBySelf(_))
    }

    pub const fn is_func(&self) -> bool {
        matches!(self, Self::Func(_))
    }

    pub fn is_constant(&self) -> bool {
        // TODO: Self::Some(_), Self::Tuple(_), Self::List(_)
        matches!(
            self,
            Self::Int(_) | Self::UInt(_) | Self::Bool(_) | Self::None | Self::Unit,
        )
    }
}

/// A [De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index)
// TODO: Add parameter indices
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index {
    lambda: usize,
}

impl Index {
    pub const fn is_zero(&self) -> bool {
        self.lambda == 0
    }

    pub fn decrement(self) -> Self {
        let lambda = self
            .lambda
            .checked_sub(1)
            .expect("attempted to decrement an index of level zero");

        Self { lambda }
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('#')?;
        Display::fmt(&self.lambda, f)
    }
}

impl FromStr for Index {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let string = string
            .strip_prefix("#")
            .ok_or_else(|| format!("expected '#<int>', got '{}'", string))?;

        let lambda = string
            .parse::<usize>()
            .map_err(|err| format!("lambda must be an integer: {:?}", err))?;

        Ok(Self { lambda })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Input {
    id: usize,
}

impl Display for Input {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("input")?;
        Display::fmt(&self.id, f)
    }
}

impl FromStr for Input {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let string = string
            .strip_prefix("input")
            .ok_or_else(|| format!("expected 'input<int>', got '{}'", string))?;

        let id = string
            .parse::<usize>()
            .map_err(|err| format!("id must be an integer: {:?}", err))?;

        Ok(Self { id })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Output {
    id: usize,
}

impl Display for Output {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("output")?;
        Display::fmt(&self.id, f)
    }
}

impl FromStr for Output {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let string = string
            .strip_prefix("output")
            .ok_or_else(|| format!("expected 'output<int>', got '{}'", string))?;

        let id = string
            .parse::<usize>()
            .map_err(|err| format!("id must be an integer: {:?}", err))?;

        Ok(Self { id })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Func {
    id: usize,
}

impl Display for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("func")?;
        Display::fmt(&self.id, f)
    }
}

impl FromStr for Func {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let string = string
            .strip_prefix("func")
            .ok_or_else(|| format!("expected 'func<int>', got '{}'", string))?;

        let id = string
            .parse::<usize>()
            .map_err(|err| format!("id must be an integer: {:?}", err))?;

        Ok(Self { id })
    }
}
