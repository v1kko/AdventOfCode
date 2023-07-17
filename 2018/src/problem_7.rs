use std::fs;
use std::collections::BinaryHeap;
use std::cmp::Reverse;
use rayon::prelude::*;
use lazy_static::lazy_static;

lazy_static! {
  static ref INSTRUCTIONS : Vec<(char,char)>  = { 
    let txt         = fs::read_to_string("7.txt").unwrap(); 
    txt[..txt.len()-1].split('\n')
      .collect::<Vec<&str>>()
      .into_par_iter()
      .map(|v| {
        let split : Vec<&str> = v.split(" ").collect();
        let req : char = split.get(1).unwrap().chars().next().unwrap();
        let dep : char = split.get(7).unwrap().chars().next().unwrap();
        (req,dep)
      }).collect::<Vec<(char,char)>>()
  };
}

pub fn a() {
  let mut instructions = INSTRUCTIONS.clone();
  let mut chars : Vec<char> = ('A'..='Z').collect();
  while chars.len() > 0 {
    for ch in chars.iter() {
      let independent : bool = instructions.iter().fold(true, |acc, instruction| {
        if *ch == instruction.1 {
          false
        } else {
          acc
        }
      });
      if independent{
        print!("{}", ch);
        instructions = instructions.into_iter().filter(|instruction| instruction.0 != *ch ).collect();
        chars = chars.clone().into_iter().filter(|chaar| *chaar != *ch).collect();
        break;
      }
    }
  }
  println!();
}

pub fn b() {
  let mut instructions = INSTRUCTIONS.clone();
  let mut chars : Vec<char> = ('A'..='Z').collect();
  let mut events : BinaryHeap<(Reverse<i32>,Option<char>)> = BinaryHeap::from([(Reverse(0),None)]);
  let mut avail = 4;
  let mut time = 0;

  while events.len() > 0 {
    let event = events.pop().unwrap();
    time = event.0.0;
    let event = event.1;
    if let Some(ch) = event {
      instructions = instructions.into_iter().filter(|instruction| instruction.0 != ch ).collect();
    }
    avail += 1;
    for _ in 0..avail {
      for ch in chars.iter() {
        let independent : bool = instructions.iter().fold(true, |acc, instruction| {
          if *ch == instruction.1 {
            false
          } else {
            acc
          }
        });
        if independent {
          avail -= 1;
          let newtime : i32 = time + 61 + ((*ch as u8 - 'A' as u8) as i32);
          events.push((Reverse(newtime),Some(*ch)));
          chars = chars.clone().into_iter().filter(|chaar| *chaar != *ch).collect();
          break;
        }
      }
    }
  }
  println!("{}",time);
}
