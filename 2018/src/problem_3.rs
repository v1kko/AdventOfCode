use std::fs;
use rayon::prelude::*;
use std::collections::HashMap;
use lazy_static::lazy_static;

#[derive(Debug)]
struct Patch {
  id: i32,
  x0: i32,
  y0: i32,
  x1: i32,
  y1: i32,
}

lazy_static! {
  #[derive(Debug)]
  static ref CONTENTS : Vec<Patch> = { 
    let txt         = fs::read_to_string("3.txt").unwrap(); 
    txt[..txt.len()-1]
      .split("\n")
      .collect::<Vec<&str>>()
      .into_par_iter()
      .map(|v| {
        let v_split = v.split(' ').collect::<Vec<&str>>();
        let id      = v_split[0][1..].parse::<i32>().unwrap();
        let start_str = v_split[2].split(',').collect::<Vec<&str>>();
        let size_str =  v_split[3].split('x').collect::<Vec<&str>>();

        let x0: i32 = start_str[0].parse().unwrap();
        let y0: i32 = start_str[1][..start_str[1].len()-1].parse().unwrap();

        Patch{
          id: id,
          x0: x0,
          y0: y0,
          x1: x0 + size_str[0].parse::<i32>().unwrap(),
          y1: y0 + size_str[1].parse::<i32>().unwrap(),
        }
      }).collect()
  };
}

pub fn a() {
    let mut overlaps : HashMap<(i32,i32),i32> = HashMap::new();
    for patch in CONTENTS.iter() {
      for i in patch.x0..patch.x1 {
        for j in patch.y0..patch.y1 {
          *overlaps.entry((i,j)).or_default() += 1;
        }
      }
    }
    let overlap: i32 = overlaps
      .into_values()
      .collect::<Vec<i32>>()
      .into_par_iter()
      .fold(|| 0,|acc, x| {
        if x > 1 {
          acc + 1
        } else {
          acc
        }
      }).sum();
    println!("{}",overlap);
}

pub fn b() {
  CONTENTS
    .par_iter()
    .map(|patch1| {
      'exit: loop {
        for patch2 in CONTENTS.iter() {
          if patch1.id == patch2.id {
            continue;
          }
          if patch1.x0 >= patch2.x1 ||
             patch2.x0 >= patch1.x1 ||
             patch1.y0 >= patch2.y1 ||
             patch2.y0 >= patch1.y1 {
          } else {
            break 'exit;
          }
        }
        println!("{}",patch1.id);
        break 'exit;
      }
    }).count();
}
