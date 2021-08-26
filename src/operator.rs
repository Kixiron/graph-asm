use egg::{Id, Symbol};
use std::{
    fmt::{self, Display, Formatter, Write},
    str::FromStr,
};

// Nodes we need:
// - looping
//   - Phi nodes
//   - Theta nodes
// - filter_arrangement: Corresponds to `Arranged::filter()`
// - flat_map: Corresponds both to `Collection::flat_map()` and `Arranged::flat_map_ref()`
// - inspect: Corresponds to `Collection::inspect()`
// - inspect_batch: Corresponds to `Collection::inspect_batch()`
// - delay: Corresponds to `Collection::delay()`
// - delay_batch: Corresponds to `Collection::delay_batch()`
// - map_difference: Corresponds to `Collection::map_difference()`
// - join_function: Corresponds to `Collection::join_function()`
//   - Figure out the applications of this irt optimization, see
//     https://github.com/frankmcsherry/blog/blob/master/posts/2021-04-26.md#generalizing-linear-operators
// - map_in_place: A reification of mapping, could allow some good opts by reducing the amount
//   of data passed around
// - exchange: Explicit exchanges? Could allow replacing some exchange contracts on arranges
//   with pipelines in the case that only the timestamp and/or difference are affected and not
//   the actual data (meaning that the key hasn't changed and therefore the exchange is redundant)
// - Scopes: Need scope entrance/exiting, fusing enters/exits with filters/maps as well as figuring
//   out how scopes will work in and of themselves. Maybe represent them primitively like timely
//   does in the background with explicit `feedback()` operators? Additionally, hoisting
//   loop/scope invariant code from within them could be really impactful, things like arranging
//   and then entering a scope could both allow more sharing and reduce the amount of churn
//   and communication that timely/ddflow experience
// - explode: Hoisting data into the difference type is a *huge* deal for performance, it can
//   have massive performance impacts for common idioms like summing up a stream. We should try
//   as hard as we can to convert reductions into explodes
// - Delta joins
// - Half joins
// - Differentiation
// - Distinct
// - Count
// - Turning things like `.distinct()` or `.count()` into `.distinct_total()` or `.count_total()` when possible
egg::define_language! {
    pub enum Operator {
        // An input operator
        Input(Input),
        // An output operator
        // FIXME: This should be `(output ?collection ?output_func)`
        //        so that we can eliminate unused things
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

        // TODO: Lots of these don't need to be intrinsic,
        //       they can just be implicit by using a catch-all case
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

        "unit" = Unit,

        // `(and ?cond1 ?cond2)`
        "and" = And([Id; 2]),
        // `(or ?cond1 ?cond2)`
        "or" = Or([Id; 2]),
        // `(not ?x)`
        // Functions both as a logical and a bitwise not
        "not" = Not(Id),

        "add" = Add([Id; 2]),
        "sub" = Sub([Id; 2]),
        "mul" = Mul([Id; 2]),
        "div" = Div([Id; 2]),
        "neg" = Neg(Id),

        "shl" = Shl([Id; 2]),
        "shr" = Shr([Id; 2]),
        "xor" = BitXor([Id; 2]),
        // TODO: Can/should logical and & logical or be overloaded
        //       to function as bitwise or & bitwise and?
        "bitor" = BitOr([Id; 2]),
        "bitand" = BitAnd([Id; 2]),

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

        // Currently contains:
        // -`(untuple ?idx:uint ?tuple:tuple)`
        // - `(typed ?type:type ?value)` where `?type` is a type name
        //   within a `Symbol` node (`i64`, `u64`, `bool`, etc.)
        // - `(do_while ?body ?while)` A tail-controlled loop
        // - `(phi ..?args)` A phi node
        Other(Symbol, Vec<Id>),
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

    pub const fn as_uint(&self) -> Option<u64> {
        if let Self::UInt(uint) = *self {
            Some(uint)
        } else {
            None
        }
    }

    pub const fn as_int(&self) -> Option<i64> {
        if let Self::Int(int) = *self {
            Some(int)
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
                // FIXME: Once we decompose reduce into `.reduce_abelian()`
                // the output of `.reduce_abelian()` is arranged
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

    pub const fn is_concat(&self) -> bool {
        matches!(self, Self::Concat(_))
    }

    pub const fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
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
            .strip_prefix('#')
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
