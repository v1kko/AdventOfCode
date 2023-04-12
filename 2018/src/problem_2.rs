use std::fs;
use rayon::prelude::*;
use std::collections::HashMap;

pub fn a() {
    let file_path        = "2.txt";
    let contents         = fs::read_to_string(file_path).unwrap();
    let contents         = &contents[..contents.len() -1];
    let boxes: _         = contents.split("\n").map(|x| x.chars().collect()).collect::<Vec<Vec<char>>>();
    let (twos, threes) : (i32, i32) 
                         = boxes.par_iter().map(|letters| {
      let mut freqs: HashMap<char, i32> 
                         = HashMap::new();
      for letter in letters {
        *freqs.entry(*letter).or_default() += 1;
      }
      let twos:   i32      = if freqs.values().any(|x| *x == 2) { 1 } else { 0 };
      let threes: i32      = if freqs.values().any(|x| *x == 3) { 1 } else { 0 };
      return (twos, threes);
    }).reduce(|| (0,0), |acc, x| ( acc.0 + x.0, acc.1 + x.1));     
    println!("{}",twos*threes);
}

pub fn b() {
    let file_path        = "2.txt";
    let contents         = fs::read_to_string(file_path).unwrap();
    let contents         = &contents[..contents.len() -1];
    let boxes: _         = contents.split("\n").map(|x| x.chars().collect()).collect::<Vec<Vec<char>>>();
    let mut pairs : Vec<(&Vec<char>,&Vec<char>)> 
                         = Vec::with_capacity(boxes.len().pow(2)-boxes.len());

    for (i, box1) in  boxes.iter().enumerate() {
      for box2 in boxes[i..].iter() {
        pairs.push((box1,box2));
      }
    }

    pairs.par_iter().map(|pair| {
      let mut count = 0;
      let mut place = 0;
      for i in 0..pair.0.len() {
        if pair.0[i] != pair.1[i] {
          count += 1;
          place = i
        }
      }
      if count == 1 {
        println!("{}", String::from_iter(pair.0[..place].iter().chain(pair.0[place+1..].iter())));
      }
    }).count();
}
