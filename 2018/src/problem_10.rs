use std::fs;
use std::cell::RefCell;
use lazy_static::lazy_static;
use rayon::prelude::*;

thread_local!(static CNT: RefCell<u32>=RefCell::new(0));

lazy_static! {
  static ref DATA : Vec<((i32,i32),(i32,i32))>  = { 
    let txt         = fs::read_to_string("10.txt").unwrap(); 
    txt[..txt.len()-1]
      .split('\n')
      .collect::<Vec<&str>>()
      .into_par_iter()
      .map(|v| {
        ((v[10..16].trim().parse().unwrap(),
          v[18..24].trim().parse().unwrap()),
         (v[36..38].trim().parse().unwrap(),
          v[40..42].trim().parse().unwrap())
         )
      })
     .collect()
  };
}

fn bounding_box(data :&Vec<((i32,i32),(i32,i32))>) -> (i32,i32,i32,i32) {

  let xs = data[0].0.0;
  let ys = data[0].0.1;

  return data.par_iter().map(|((x,y),(_,_))| (*x,*x,*y,*y))
    .reduce(|| (xs,xs,ys,ys), | a,b | (a.0.min(b.0),a.1.max(b.1),a.2.min(b.2), a.3.max(b.3)));
}

pub fn a() {
  a_inner(true);
}

pub fn a_inner(print:bool) {
  let mut data : Vec<((i32,i32),(i32,i32))>= DATA.clone();
  let (x0,x1,y0,y1) = bounding_box(&data);
  let mut min = (x1-x0).abs().min((y1-y0).abs());
  let mut old_data = data.clone();
  let factor = 100;
   
  CNT.with(|inner_cnt| {
    *inner_cnt.borrow_mut() = 0;

    loop {
      let old_old_data = old_data.clone();
      old_data = data.clone();
      data = data.par_iter().map(|((x,y),(vx,vy)) | ((x+vx*factor,y+vy*factor),(*vx,*vy))).collect();
      let (x0,x1,y0,y1) = bounding_box(&data);
      let new_min = (x1-x0).abs().min((y1-y0).abs());
      if new_min > min {
        data = old_old_data;
        break;
      }
      *inner_cnt.borrow_mut() += factor as u32;
      min = new_min;
    }
    *inner_cnt.borrow_mut() -= factor as u32;

    let (x0,x1,y0,y1) = bounding_box(&data);
    min = (x1-x0).abs().min((y1-y0).abs());



    loop {
      // TODO implement binary search
      let old_data = data.clone();
      data = data.par_iter().map(|((x,y),(vx,vy)) | ((x+vx,y+vy),(*vx,*vy))).collect();
      let (x0,x1,y0,y1) = bounding_box(&data);
      let new_min = (x1-x0).abs().min((y1-y0).abs());
      if new_min > min {
        data = old_data;
        break;
      }
      *inner_cnt.borrow_mut() += 1;
      min = new_min;
    }
  });
  let (x0,x1,y0,y1) = bounding_box(&data);
  let xmin = x0.min(x1);
  let ymin = y0.min(y1);
  let xlen = (x1-x0).abs()+1;
  let ylen = (y1-y0).abs()+1;
  let mut state = vec![vec![' '; xlen as usize]; ylen as usize];
  data.iter().for_each(|((x,y),(_,_))| {state[(*y - ymin) as usize][(*x -xmin) as usize] = '#';});
  if ! print { return; }
  println!();
  for x in 0..ylen {
    let s : String = state[x as usize].iter().collect();
    println!("{}",s);
  }
}

pub fn b() {
  CNT.with(|inner_cnt| { 
    if *inner_cnt.borrow() == 0 {
      a_inner(false);
    }
    println!("{}",inner_cnt.borrow()) 
  });
}
