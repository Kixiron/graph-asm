use crate::Runner;
use std::fs;

pub fn svg(runner: &Runner, name: &str) {
    let dot = runner.egraph.dot();
    let _ = fs::create_dir_all(concat!(env!("CARGO_MANIFEST_DIR"), "/target/graphs"));
    let _ = dot.to_svg(format!(
        concat!(env!("CARGO_MANIFEST_DIR"), "/target/graphs/{}-{}.svg"),
        name,
        runner.iterations.len(),
    ));
}

pub fn png(runner: &Runner, name: &str) {
    let dot = runner.egraph.dot();
    let _ = fs::create_dir_all(concat!(env!("CARGO_MANIFEST_DIR"), "/target/graphs"));
    let _ = dot.to_png(format!(
        concat!(env!("CARGO_MANIFEST_DIR"), "/target/graphs/{}-{}.png"),
        name,
        runner.iterations.len(),
    ));
}
