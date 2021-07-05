mod contains_function;
mod inline_unused_let;
mod prune;
mod variable_propagation;

pub use contains_function::contains_function_call;
pub use inline_unused_let::inline_unused_let;
pub use prune::prune;
pub use variable_propagation::variable_propagation;
