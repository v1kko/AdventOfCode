use std::fs;
use std::array::from_fn;
use lazy_static::lazy_static;
use rayon::prelude::*;

lazy_static! {
  static ref DATA : i32  = { 
    let txt         = fs::read_to_string("11.txt").unwrap(); 
    txt[..txt.len()-1].parse().unwrap()
  };
}

pub fn a() {
  let serial : i32 = DATA.clone();

  let best: (usize,usize, i32) = (1..299)
    .flat_map(move |x| (1..299).map(move |y| (x,y)))
    .collect::<Vec<(usize,usize)>>()
    .par_iter()
    .map(|(x,y)| { 
      let mut total : i32 = 0;
      for xx in *x..*x+3 {
        for yy in *y..*y+3 {
          let rack_id : i32 = xx as i32 + 10;
          let mut power_level : i32 = rack_id * yy as i32;
          power_level += serial;
          power_level = power_level * rack_id;
          power_level = (power_level / 100) % 10;
          power_level = power_level - 5;
          total += power_level
        }
      }
      (*x, *y, total)
    }).reduce(|| (0, 0, -1), | a, b | {
      if a.2 > b.2 { a } else { b }
    });

  println!("{},{}",best.0, best.1);
}

pub fn b() {
  let serial : i32 = DATA.clone();

  let grid : [[i32; 300]; 300] = from_fn(|x| from_fn(|y| {
      let rack_id : i32 = x as i32 + 11;
      let mut power_level : i32 = rack_id * (y+1) as i32;
      power_level += serial;
      power_level = power_level * rack_id;
      power_level = (power_level / 100) % 10;
      power_level = power_level - 5;
      power_level
  }));

  let best: (usize,usize, usize, i32) = (0..301)
    .flat_map(move |x| (0..301)
    .flat_map(move |y| {
      let max_size = (301-x).min(301-y);
      (1..max_size).map(move |z| (x,y,z))
    }))
    .collect::<Vec<(usize,usize,usize)>>()
    .par_iter()
    .map(|(x,y,z)| { 
      let mut total : i32 = 0;
      for xx in *x..*x+z {
        for yy in *y..*y+z {
          total += grid[xx][yy];
        }
      }
      (*x+1,*y+1,*z, total)
    }).reduce(|| (0, 0, 0, -1), | a, b | {
      if a.3 > b.3 { a } else { b }
    });
  println!("{},{},{}",best.0, best.1, best.2);
}
