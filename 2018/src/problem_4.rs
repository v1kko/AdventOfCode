use std::fs;
use rayon::prelude::*;
use std::collections::HashMap;
use lazy_static::lazy_static;
use chrono::{NaiveDateTime,Timelike};
use core::cmp::Ordering;


#[derive(Debug, Eq, PartialEq)]
enum Action {
  Begins,
  Awakens,
  Sleeps,
}

#[derive(Debug)]
struct Event {
  guard_id: Option<i32>,
  time: NaiveDateTime,
  action: Action,
}
impl PartialEq for Event {
  fn eq(&self, other: &Self) -> bool {
    self.time == other.time
  }
}
impl Eq for Event { }
impl PartialOrd for Event {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.time.partial_cmp(&other.time)
  }
}
impl Ord for Event {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.cmp(&other.time)
    }
}


lazy_static! {
  #[derive(Debug)]
  static ref CONTENTS : HashMap<i32,[i32;60]>  = { 
    let txt         = fs::read_to_string("4.txt").unwrap(); 
    let mut parsed  = txt[..txt.len()-1].split("\n")
      .collect::<Vec<&str>>()
      .into_par_iter()
      .map(|v| {
        let split = v.split("] ").collect::<Vec<&str>>();
        let time = NaiveDateTime::parse_from_str(&split[0][1..], "%Y-%m-%d %H:%M").unwrap();
        let action = {
          match split[1].chars().next().unwrap() {
            'G' => Action::Begins,
            'w' => Action::Awakens,
            'f' => Action::Sleeps,
             _  => panic!("This should not happen")
          }
        };
        let guard_id = 
          if action == Action::Begins {
            split[1].split(' ').collect::<Vec<&str>>()[1][1..].parse::<i32>().ok()
          } else {
            None
          };
        Event {
          guard_id: guard_id,
          time: time,
          action: action,
        }
      })
      .collect::<Vec<Event>>();
    parsed.par_sort();
    let mut guard: Option<i32> = None;
    for event in parsed.iter_mut() {
      if event.action == Action::Begins {
        guard = event.guard_id
      } else {
        event.guard_id = guard
      }
    }

    let mut schedule : HashMap<i32,[i32;60]> = HashMap::new();
    let mut start : usize = 70;

    for event in parsed.iter() {
      match event.action {
        Action::Sleeps => {
          start = event.time.minute() as usize;
        },
        Action::Awakens => {
          let stop: usize = event.time.minute() as usize;
          schedule.entry(event.guard_id.unwrap()).or_insert([0;60])[start..stop].iter_mut().for_each(|v| *v += 1);
          start = 70;
        },
        Action::Begins => {
          start = 70;
        },
      }
    }

    let schedule = schedule;
    schedule
  };
}

pub fn a() {
  let schedule = &CONTENTS;
  let (laziest_guard, _how_lazy) = schedule.iter().fold((0,0), | acc, guard | {
    let sleep = guard.1.iter().sum();
    if sleep > acc.1 {
      (*guard.0,sleep)
    } else {
      acc
    }
  });
  let (sleep_time, _most_sleep) = schedule[&laziest_guard].iter().enumerate().fold((0,0), | acc, x| {
    if x.1 > &acc.1 {
      (x.0, *x.1)
    } else {
      acc
    }
  });
  println!("{}", laziest_guard*sleep_time as i32);
}

pub fn b() {
  let schedule = &CONTENTS;
  let (laziest_guard, _laziness, minute) = schedule.iter().fold((0,0,0), | acc, guard | {
    let laziness = guard.1.iter().enumerate().reduce(|acc, v| {
      if v.1 > acc.1 {
        v
      } else {
        acc
      }
    }).unwrap();
    if *laziness.1 > acc.1 {
      (*guard.0, *laziness.1, laziness.0 as i32)
    } else {
      acc
    }
  });
  println!("{}", laziest_guard*minute);
}
