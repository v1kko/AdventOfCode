use std::fs;
use rayon::prelude::*;
use std::collections::HashMap;

pub fn a() {
    let file_path = "1.txt";
    let contents = fs::read_to_string(file_path).unwrap();
    let frequencies: _ = contents.split("\n").collect::<Vec<&str>>();
    let result: i32 = frequencies.par_iter().cloned().fold(|| 0, |acc, x| { 
      let y = x.parse::<i32>();
      match y {
        Ok(x) => acc + x,
        Err(_e) => acc
      }
    }).sum();
    println!("{}",result);
}

pub fn b() {
    let file_path = "1.txt";
    let contents = fs::read_to_string(file_path).unwrap();
    let steps: _ = contents.split("\n").collect::<Vec<&str>>();
    let steps = steps.par_iter().cloned().filter_map(|x| x.parse::<i32>().ok()).collect::<Vec<i32>>();
    let mut visited = HashMap::new();
    let mut cur: i32 = 0;
    visited.insert(cur,true);
    'outer: loop {
        for step in &steps {
            cur += step;
            if visited.get(&cur).is_some() { 
                break 'outer; 
            };
            visited.insert(cur,true);
        }
    }
    println!("{}",cur);
}
