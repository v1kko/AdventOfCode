use std::fs;
use lazy_static::lazy_static;
use rayon::prelude::*;

lazy_static! {
  static ref DATA : Vec<i32>  = { 
    let txt         = fs::read_to_string("8.txt").unwrap(); 
    txt[..txt.len()-1].split(' ')
      .collect::<Vec<&str>>()
      .into_par_iter()
      .map(|v| {
          v.parse::<i32>().unwrap()
      }).collect::<Vec<i32>>()
  };
}



fn sum_metadata(data : &mut std::vec::IntoIter<i32>) -> i32{
  let childs = data.next().unwrap();
  let data_length = data.next().unwrap();
  let mut sum : i32 = 0;
  for _ in 0..childs {
    sum += sum_metadata(data);
  }
  for _ in 0..data_length {
    sum += data.next().unwrap();
  }
  return sum;
}

pub fn a() {
  let data : Vec<i32> = DATA.clone();
  let mut iter = data.into_iter();
  let sum = sum_metadata(&mut iter);
  println!("{:?}",sum);
}

fn sum_complicated(data : &mut std::vec::IntoIter<i32>) -> i32{
  let childs = data.next().unwrap();
  let data_length = data.next().unwrap();
  let mut sum : i32 = 0;
  if childs == 0 {
    for _ in 0..data_length {
      sum += data.next().unwrap();
    }
  } else {
    let childs : Vec<i32> = (0..childs).map(|_|sum_complicated(data)).collect();
    for _ in 0..data_length {
      let child : usize = (data.next().unwrap() - 1) as usize;
      if let Some(value) = childs.get(child) {
        sum += *value;
      }
    }
  }
  return sum;
}

pub fn b() {

  let data : Vec<i32> = DATA.clone();
  let mut iter = data.into_iter();
  let sum = sum_complicated(&mut iter);
  println!("{:?}",sum);
}
