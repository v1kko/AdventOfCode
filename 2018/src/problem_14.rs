use std::fs;
use lazy_static::lazy_static;

lazy_static! {
  static ref DATA : usize  = { 
    let txt         = fs::read_to_string("14.txt").unwrap(); 
    txt[..txt.len()-1].parse::<usize>().unwrap()
  };
}

pub fn a() {
  let goal : usize = DATA.clone();
  let mut recipes : Vec<usize> = vec![3,7];
  let mut elf1 = 0;
  let mut elf2 = 1;
  while recipes.len() < goal + 10 {
    let recipe = recipes[elf1]+recipes[elf2];
    if recipe / 10 > 0 {
      recipes.push(1);
    } 
    recipes.push(recipe%10);
    elf1 += recipes[elf1] + 1;
    elf1 %= recipes.len();
    elf2 += recipes[elf2] + 1;
    elf2 %= recipes.len();
  }
  println!("{}",String::from_iter(recipes[goal..goal+10].iter().map(|x| x.to_string())));
}

pub fn b() {
  let goal : usize = DATA.clone();
  let goal : Vec<usize> = goal.to_string().chars().map(|x| x.to_digit(10).unwrap() as usize).collect();
  let goal_len = goal.len();
  let mut recipes : Vec<usize> = vec![3,7];
  let mut elf1 = 0;
  let mut elf2 = 1;
  loop {
    let recipe = recipes[elf1]+recipes[elf2];
    if recipe / 10 > 0 {
      recipes.push(1);
      if recipes.len() >= goal_len {
        if recipes[recipes.len()-goal_len..recipes.len()] == goal {
          break;
        }
      }
    } 
    recipes.push(recipe%10);
    if recipes.len() >= goal_len {
      if recipes[recipes.len()-goal_len..recipes.len()] == goal {
        break;
      }
    }
    elf1 += recipes[elf1] + 1;
    elf1 %= recipes.len();
    elf2 += recipes[elf2] + 1;
    elf2 %= recipes.len();
  }
  println!("{}",recipes.len()-goal_len);

}
