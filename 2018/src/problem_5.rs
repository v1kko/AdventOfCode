use std::fs;
use rayon::prelude::*;
use lazy_static::lazy_static;
use arr_macro::arr;

lazy_static! {
  #[derive(Debug)]
  static ref CONTENTS : Vec<i16>  = { 
    let txt         = fs::read_to_string("5.txt").unwrap(); 
    txt[..txt.len()-1].bytes().map(|x| x as i16).collect()
  };
}

pub fn a() {
  let polymer = &CONTENTS;
  let mut stack : Vec<i16> = Vec::with_capacity(polymer.len());
  for element in polymer.iter() {
    let prev = stack.last();
    if prev == None { 
      stack.push(*element);
      continue;
    }

    let prev = prev.unwrap();
    
    if prev + 32 == *element || prev - 32 == *element {
      stack.pop();
    } else {
      stack.push(*element)
    }
  }

  println!("{}", stack.len());
}

pub fn b() {
  let polymer = &CONTENTS;
  let mut stacks : [Vec<i16>;26] = arr![Vec::with_capacity(polymer.len());26];
  for element in polymer.iter() {
    for (i, stack) in stacks.iter_mut().enumerate() {
      if element - 65 == i as i16 || element - 97 == i as i16 {
        continue;
      }

      let prev = stack.last();
      if prev == None { 
        stack.push(*element);
        continue;
      }

      let prev = prev.unwrap();
      
      if prev + 32 == *element || prev - 32 == *element {
        stack.pop();
      } else {
        stack.push(*element)
      }
    }
  }

  println!("{}", stacks.into_par_iter().min_by_key(|x| x.len()).unwrap().len());
}
