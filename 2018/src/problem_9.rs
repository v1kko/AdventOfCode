use std::rc::{Rc,Weak};
use std::cell::RefCell;
use std::fs;
use lazy_static::lazy_static;
use std::fmt;

lazy_static! {
  static ref DATA : (u32,u32)  = { 
    let txt         = fs::read_to_string("9.txt").unwrap(); 
    let split = txt[..txt.len()-1].split(' ').collect::<Vec<&str>>();
    (split[0].parse::<u32>().unwrap(),split[6].parse::<u32>().unwrap())
  };
}

pub struct Node {
    id: u32,
    prev: Weak<RefCell<Node>>,
    next: Weak<RefCell<Node>>
}
impl Node {
  fn remove(& self) -> u32 {
    let prev : Weak<RefCell<Node>> = self.prev.clone();
    let next : Weak<RefCell<Node>> = self.next.clone();

    prev.upgrade().unwrap().borrow_mut().next = next.clone();
    next.upgrade().unwrap().borrow_mut().prev = prev.clone();
    self.id
  }
}

#[derive(Debug)]
pub struct Game {
    cur : Weak<RefCell<Node>>,
    cells: Vec<Rc<RefCell<Node>>>,
    players: u32,
    score: Vec<u32>,
}

impl Game {
  fn new(players:u32) -> Game {
    let  zero = Rc::new_cyclic(|node| RefCell::new(Node{id:0,prev:node.clone(), next: node.clone()}));
    let mut cells = Vec::new();
    cells.push(zero);
    Self
    {
      cur:Rc::downgrade(&cells[0]).clone(), 
      cells : cells,
      players : players,
      score : vec![0;players as usize]
    }
  } 

  fn insert(& mut self, id: u32) {
    let prev : Weak<RefCell<Node>> = self.cur.upgrade().unwrap().borrow().next.clone();
    let next : Weak<RefCell<Node>> = prev.upgrade().unwrap().borrow().next.clone();
    let node : Rc<RefCell<Node>>   = Rc::new(RefCell::new(Node{id:id, prev:prev.clone(), next:next.clone()}));
    next.upgrade().unwrap().borrow_mut().prev = Rc::downgrade(&node);
    prev.upgrade().unwrap().borrow_mut().next = Rc::downgrade(&node);
    self.cur = Rc::downgrade(&node);
    self.cells.push(node);
  }

  fn play(& mut self, turns:u32) {
    for x in 1..turns+1 {
      if x%23 == 0 {
        for _ in 0..7 {
          self.cur = self.cur.upgrade().unwrap().borrow().prev.clone();
        }
        let score = self.cur.upgrade().unwrap().borrow().remove() + x;
        self.cur = self.cur.upgrade().unwrap().borrow().next.clone();
        self.score[(x%self.players) as usize] += score;
      } else {
        self.insert(x);
      }
    }
  }
}

pub fn a() {
  let (players,turns) = *DATA;
  let mut game = Game::new(players);
  game.play(turns);
  println!("{}", game.score.iter().max().unwrap());
}

pub fn b() {
  let (players,turns) = *DATA;
  let mut game = Game::new(players);
  game.play(turns*100);
  println!("{}", game.score.iter().max().unwrap());
}

// Extras
impl fmt::Debug for Node {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[{} < {} > {}]", 
           self.prev.upgrade().unwrap().borrow().id, 
           self.id, 
           self.next.upgrade().unwrap().borrow().id)
  }
}
impl fmt::Display for Game {
  // This trait requires `fmt` with this exact signature.
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let start : u32 = self.cur.upgrade().unwrap().borrow().id;
    write!(f, "{}", start)?;
    let mut cur = self.cur.upgrade().unwrap().borrow().next.clone();
    let mut id : u32 = cur.upgrade().unwrap().borrow().id;
    while id != start {
      write!(f, " {}", id)?;
      cur = cur.upgrade().unwrap().borrow().next.clone();
      id = cur.upgrade().unwrap().borrow().id;
    }
    Ok(())
  }
}

