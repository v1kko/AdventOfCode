use std::fs;
use rayon::prelude::*;
use std::collections::HashMap;

pub fn a() {
    let file_path = "2.txt";
    let contents = fs::read_to_string(file_path).unwrap();
    let boxes: _ = contents.split("\n").map(|x| x.chars().collect()).collect::<Vec<Vec<char>>>();
    let (twos, threes) : (i32, i32) = boxes.par_iter().map(|letters| {
        let mut freqs: HashMap<char, i32> = HashMap::new();
            for letter in letters {
                *freqs.entry(*letter).or_default() += 1;
            }
            let twos:   i32 = if freqs.values().any(|x| *x == 2) { 1 } else { 0 };
            let threes: i32 = if freqs.values().any(|x| *x == 3) { 1 } else { 0 };
            return (twos, threes);
        }).reduce(|| (0,0), |acc, x| ( acc.0 + x.0, acc.1 + x.1));     
    println!("{}",twos*threes);
}

pub fn b() {
   
}
