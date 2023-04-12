use std::fs;
use rayon::prelude::*;
use std::collections::HashMap;

pub fn a() {
    let file_path        = "1.txt";
    let contents         = fs::read_to_string(file_path).unwrap();
    let contents         = &contents[..contents.len() -1];
    let frequencies: _   = contents.split("\n").collect::<Vec<&str>>();
    let frequencies: _   = frequencies.par_iter().map(|x| x.parse::<i32>().unwrap());
    let result     : i32 = frequencies.reduce(|| 0, |acc, x| acc+x);
    println!("{}",result);
}

pub fn b() {
    let file_path        = "1.txt";
    let contents         = fs::read_to_string(file_path).unwrap();
    let contents         = &contents[..contents.len() -1];
    let steps: _         = contents.split("\n").collect::<Vec<&str>>();
    let steps: Vec<i32>  = steps.par_iter().map(|x| x.parse::<i32>().unwrap()).collect::<Vec<i32>>();
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
