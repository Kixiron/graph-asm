use crate::Runner;
use std::{cmp::Reverse, collections::HashMap};

pub fn debug_topk(runner: &Runner) {
    if let Some(iter) = runner.iterations.last() {
        let mut applied_rewrites: Vec<_> = iter.applied.iter().collect();
        applied_rewrites.sort_unstable_by_key(|&(_, &times)| Reverse(times));

        if !applied_rewrites.is_empty() {
            println!("iter {} {{", runner.iterations.len());
            for (rewrite, times) in applied_rewrites {
                println!("    {}: {}", times, rewrite);
            }

            let mut total_rewrites: Vec<_> = runner
                .iterations
                .iter()
                .flat_map(|iter| iter.applied.iter())
                .fold(HashMap::new(), |mut map, (rewrite, &times)| {
                    map.entry(rewrite)
                        .and_modify(|total| *total += times)
                        .or_insert(times);
                    map
                })
                .into_iter()
                .collect();
            total_rewrites.sort_unstable_by_key(|&(_, times)| Reverse(times));

            if !total_rewrites.is_empty() {
                println!("\n totals:");
                for (rewrite, times) in total_rewrites {
                    println!("    {}: {}", times, rewrite);
                }
            }

            println!("}}");
        }
    }
}
