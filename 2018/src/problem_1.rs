use std::fs;
use std::collections::HashSet;
use lazy_static::lazy_static;

lazy_static! {
  static ref CONTENTS : Vec<i32> = { 
    let raw         = fs::read_to_string("1.txt").unwrap(); 
    let raw         = &raw[..raw.len()-1];
    let deltas_str  = raw.split("\n").collect::<Vec<&str>>();
    let deltas      = deltas_str.iter().map(|x| x.parse::<i32>().unwrap());
    deltas.collect::<Vec<i32>>()
  };
}

pub fn a() {
    let result     : i32 = CONTENTS.iter().fold( 0,|acc, x| acc+x);
    println!("{}",result);
}

pub fn b() {

    let mut visited = HashSet::new();
    let mut cur: i32 = 0;
    visited.insert(cur);
    'outer: loop {
        for step in &*CONTENTS {
            cur += step;
            if visited.contains(&cur) { 
                break 'outer; 
            };
            visited.insert(cur);
        }
    }
    println!("{}",cur);
}
