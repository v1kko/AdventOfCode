use std::fs;
use std::array::from_fn;
use lazy_static::lazy_static;

#[derive(Debug)]
struct Rule {
  pattern : [char; 5],
  outcome : char
} 


lazy_static! {
    static ref DATA : (Vec<char>, Vec<Rule>)  = { 
    let txt         = fs::read_to_string("12.txt").unwrap(); 
    let mut txt_it = txt[..txt.len()-1].split('\n');
    let initial_state = txt_it.next().unwrap().split(' ').collect::<Vec<&str>>()[2].chars().collect();
    txt_it.next();
    let rules : Vec<Rule> = txt_it.map(|x| {
      let tokens = x.split(' ').collect::<Vec<&str>>();
      Rule{pattern : tokens[0].chars().collect::<Vec<char>>().try_into().unwrap(), outcome : tokens[2].chars().next().unwrap()}
    }).collect();
    (initial_state, rules)
  };
}

pub fn a() {
  let initial = &DATA.0;
  let rules = &DATA.1;
  let mut new = initial.clone();
  let mut offset : i32 = 0;
  
  for _ in 0..20 {
    let old = new;
    let start = old.iter().position(|&x| x== '#').unwrap();
    let stop  = old.iter().rposition(|&x|x=='#').unwrap();
    new = vec!['.';(stop-start)+5];
    offset += 2-start as i32;
    for x in 0..new.len() {
      let pattern : [ char; 5] = from_fn(|y| {
        if x + y < 4 {
          '.'
        } else {
          *old.get((x+y)-4+start).or_else(|| Some(&'.')).unwrap()
        }
      });
      for rule in rules {
        if pattern == rule.pattern {
          new[x] = rule.outcome;
          break;
        } 
      }
    }
  }
  let count : i32 = new.into_iter().enumerate().map(|(i,x)| { if x == '#' { (i as i32) - offset } else { 0 } }).sum();
  println!("{:?}",count);
}

pub fn b() {
  let initial = &DATA.0;
  let rules = &DATA.1;
  let mut new = initial.clone();
  let mut offset : i64 = 0;
  
  for _ in 0..130 {
    let old = new;
    let start = old.iter().position(|&x| x== '#').unwrap();
    let stop  = old.iter().rposition(|&x|x=='#').unwrap();
    new = vec!['.';(stop-start)+5];
    offset += 2-start as i64;
    for x in 0..new.len() {
      let pattern : [ char; 5] = from_fn(|y| {
        if x + y < 4 {
          '.'
        } else {
          *old.get((x+y)-4+start).or_else(|| Some(&'.')).unwrap()
        }
      });
      for rule in rules {
        if pattern == rule.pattern {
          new[x] = rule.outcome;
          break;
        } 
      }
    }
  }
  // It converged, fix offset to get any further generation
  offset -= 50000000000-130;
  let count : i64 = new.iter().enumerate().map(|(i,x)| { if *x == '#' { (i as i64) - offset } else { 0 } }).sum();
  
  println!("{}", count)


}
