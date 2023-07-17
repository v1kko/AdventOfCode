use std::fs;
use rayon::prelude::*;
use lazy_static::lazy_static;
use std::collections::HashSet;

lazy_static! {
  static ref COORDS : Vec<(i32,i32)>  = { 
    let txt         = fs::read_to_string("6.txt").unwrap(); 
    txt[..txt.len()-1].split('\n')
      .collect::<Vec<&str>>()
      .into_par_iter()
      .map(|v| {
        let mut coord = v.split(", ");
        (coord.next().unwrap().parse().unwrap(), coord.next().unwrap().parse().unwrap())
      }).collect::<Vec<(i32,i32)>>()
  };

  static ref INIT_BBOX : (i32,i32,i32,i32) = (COORDS[0].0,COORDS[0].0,COORDS[0].1,COORDS[0].1);

  static ref BBOX : (i32,i32,i32,i32) = COORDS.par_iter().fold(|| *INIT_BBOX, |bbox, coord| {
    let x0 = bbox.0.min(coord.0);
    let x1 = bbox.1.max(coord.0);
    let y0 = bbox.2.min(coord.1);
    let y1 = bbox.3.max(coord.1);
    (x0,x1,y0,y1)
  }).reduce(|| *INIT_BBOX, |acc, bbox| {
    let x0 = acc.0.min(bbox.0);
    let x1 = acc.1.max(bbox.1);
    let y0 = acc.2.min(bbox.2);
    let y1 = acc.3.max(bbox.3);
    (x0,x1,y0,y1)
  });
}

pub fn a() {
  let coords = &COORDS;

  let x0 = BBOX.0;
  let x1 = BBOX.1;
  let y0 = BBOX.2;
  let y1 = BBOX.3;

  let grid = (x0..x1+1) .flat_map(|x| (y0..y1+1).map(move |y| (x,y)));
  let distances = grid.map(|point| {
      (point, 
       coords.iter().enumerate()
        .map(move |(i,coord)| (i as i32, (coord.0 - point.0).abs() + (coord.1 - point.1).abs())
             ).collect::<Vec<(i32,i32)>>()
        )
    }).collect::<Vec<((i32,i32),Vec<(i32,i32)>)>>();
  let closest : Vec<((i32,i32),Option<i32>)> = distances.iter()
    .map(| (point, distances) | {
      let mut distances = distances.clone();
      distances.sort_by(|a, b| a.1.cmp(&b.1));
      let minimal = distances.get(0).unwrap();
      if distances.get(1).unwrap().1 != minimal.1 {
        (*point,Some(minimal.0))
      } else {
        (*point,None)
      }
    }).collect();
  let mut forbidden: HashSet<i32> = HashSet::new();
  let mut score : Vec<i32> = vec![0; coords.len()]; 
  for (point, winner) in closest {
    if let Some(winner) = winner {
      if point.0 == x0 || point.0 == x1 || point.1 == y0 || point.1 == y1 {
        forbidden.insert(winner);
        continue;
      }
      *score.get_mut(winner as usize).unwrap() += 1;
    }
  }
  let highest = score.iter().enumerate().filter(|(id, _score)| ! forbidden.contains(&(*id as i32) ))
    .reduce(|acc, cur| {
      if acc.1 < cur.1 {
        cur
      } else {
        acc
      }}
    ).unwrap().1;

  println!("{}", highest);

}

pub fn b() {
  let coords = &COORDS;

  let x0 = BBOX.0;
  let x1 = BBOX.1;
  let y0 = BBOX.2;
  let y1 = BBOX.3;

  let (x0_o,x1_o,y0_o,y1_o) = coords.par_iter().fold(|| (0,0,0,0), |acc, coord| {
    (acc.0 + (coord.0 - x0),
     acc.1 + (x1 - coord.0),
     acc.2 + (coord.1 - y0),
     acc.3 + (y1 - coord.1))
  }).reduce(|| (0,0,0,0), |a, b| (a.0+b.0,a.1+b.1,a.2+b.2,a.3+b.3));

  const MAX : i32 = 10000;

  let x0 = x0 + x0_o - MAX;
  let x1 = x1 - x1_o + MAX;
  let y0 = y0 + y0_o - MAX;
  let y1 = y1 - y1_o + MAX;
  
  let safe : i32 = (x0..x1)
    .flat_map(move |x| (y0..y1)
    .map(move |y| (x,y)))
    .collect::<Vec<(i32,i32)>>()
    .par_iter()
    .fold_with(0_i32, |acc, (x,y)| {
      let score : i32 = coords.iter().fold(0, |acc, coord| acc + (coord.0 - x).abs() + (coord.1 - y).abs());
      if score < MAX {
        acc + 1
      } else {
        acc
      }
    }).sum::<i32>();

  println!("{}", safe);
}
