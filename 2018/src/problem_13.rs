use std::fs;
use std::cmp::Ordering;
use lazy_static::lazy_static;

#[derive(Clone, Debug)]
enum RailType {
  Straight{occ : bool},
  Crossing{occ : bool },
  BendForward{occ : bool },
  BendBackward{occ : bool },
  Empty,
}

impl RailType {
  fn ride( self : &mut RailType, train : &mut Train) {
    match self {
      RailType::Straight { occ } => {
        match train.orientation {
          Orientation::North => {train.x = train.x-1;},
          Orientation::South => {train.x = train.x+1;},
          Orientation::West => {train.y = train.y-1;},
          Orientation::East => {train.y = train.y+1;},
        }
        *occ = false;
      },
      RailType::BendForward { occ } => {
        match train.orientation {
          Orientation::North => {train.y = train.y+1; train.orientation = Orientation::East;},
          Orientation::South => {train.y = train.y-1; train.orientation = Orientation::West;},
          Orientation::West => {train.x = train.x+1; train.orientation = Orientation::South;},
          Orientation::East => {train.x = train.x-1; train.orientation = Orientation::North;},
        }
        *occ = false;
      },
      RailType::BendBackward { occ } => {
        match train.orientation {
          Orientation::North => {train.y = train.y-1; train.orientation = Orientation::West;},
          Orientation::South => {train.y = train.y+1; train.orientation = Orientation::East;},
          Orientation::West => {train.x = train.x-1; train.orientation = Orientation::North;},
          Orientation::East => {train.x = train.x+1; train.orientation = Orientation::South;},
        }
        *occ = false;
      },
      RailType::Crossing { occ } => {
        train.turn();
        match train.orientation {
          Orientation::North => {train.x = train.x-1;},
          Orientation::South => {train.x = train.x+1;},
          Orientation::West => {train.y = train.y-1;},
          Orientation::East => {train.y = train.y+1;},
        }
        *occ = false;
      }
      RailType::Empty => panic!("Encountered Empty square"),
    };
  }
  fn occupy( self : &mut RailType ) -> bool {
    match self {
      RailType::Empty => panic!("Encountered Empty square"),
      RailType::Crossing{ occ } 
    | RailType::Straight{ occ } 
    | RailType::BendForward{ occ }
    | RailType::BendBackward{ occ } => {
        if *occ {
          *occ = false; // Crash empties square
          false
        } else {
          *occ = true;
          true
        }
      }
    }
      
  }
}

#[derive(Clone, Debug, Copy)]
enum Orientation {
  North = 0,
  East = 1,
  South = 2,
  West = 3
}

impl TryFrom<u8> for Orientation {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == Orientation::North as u8 => Ok(Orientation::North),
            x if x == Orientation::East as u8 => Ok(Orientation::East),
            x if x == Orientation::South as u8 => Ok(Orientation::South),
            x if x == Orientation::West as u8 => Ok(Orientation::West),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, Copy)]
enum Direction {
  Left,
  Right,
  Straight
}

#[derive(Clone, Debug, Copy)]
struct Train {
  x: usize,
  y: usize,
  orientation: Orientation,
  next_turn: Direction,
}

impl Ord for Train {
  fn cmp(&self, other: &Self) -> Ordering {
    if self.x == other.x {
      self.y.cmp(&other.y)
    } else {
      self.x.cmp(&other.x)
    }
  }
}

impl PartialOrd for Train {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      Some(self.cmp(other))
  }
}

impl PartialEq for Train {
  fn eq(&self, other: &Self) -> bool {
    self.x == other.x && self.y == other.y
  }
}

impl Eq for Train {}

impl Train {
  fn ride(&mut self, map : &mut Vec<Vec<RailType>>) -> bool {
    let loc : &mut RailType = &mut map[self.x][self.y];
    loc.ride(self);
    let newloc : &mut RailType = &mut map[self.x][self.y];
    newloc.occupy()
  }

  fn turn(&mut self) {
    match self.next_turn {
      Direction::Left => {
        self.next_turn = Direction::Straight;
        self.orientation = ((self.orientation as u8 + 3) % 4).try_into().unwrap();
      },
      Direction::Straight => {
        self.next_turn = Direction::Right;
      },
      Direction::Right => {
        self.next_turn = Direction::Left;
        self.orientation = ((self.orientation as u8 + 1) % 4).try_into().unwrap();
      },
    };
  }
}

lazy_static! {
    static ref DATA : (Vec<Train>, Vec<Vec<RailType>>)  = { 
    let txt         = fs::read_to_string("13.txt").unwrap(); 
    let mut trains : Vec<Train> = Vec::new();
    let map = txt[..txt.len()-1].split('\n').enumerate().map(|(x,line)| {
      line.chars().enumerate().map(|(y, c)| {
        let typ : Option<RailType> =  match c {
          '|' =>  Some(RailType::Straight{occ : false}),
          '-' =>  Some(RailType::Straight{occ : false}),
          '/' =>  Some(RailType::BendForward{occ : false}),
          '\\'=>  Some(RailType::BendBackward{occ : false}),
          '+' =>  Some(RailType::Crossing{occ : false}),
          '>' =>  { trains.push(Train{x: x, y:y, orientation: Orientation::East, next_turn: Direction::Left});
                    Some(RailType::Straight{ occ : true}) },
          '^' =>  { trains.push(Train{x: x, y:y, orientation: Orientation::North, next_turn: Direction::Left});
                    Some(RailType::Straight{ occ : true}) },
          '<' =>  { trains.push(Train{x: x, y:y, orientation: Orientation::West, next_turn: Direction::Left});
                    Some(RailType::Straight{ occ : true}) },
          'v' =>  { trains.push(Train{x: x, y:y, orientation: Orientation::South, next_turn: Direction::Left});
                    Some(RailType::Straight{ occ : true}) },
          ' ' =>  Some(RailType::Empty),
           _  =>  { println!("I can't parse character '{}' for you, sorry",c); None },
        };
        typ.unwrap()
      }).collect::<Vec<RailType>>()
    }).collect::<Vec<Vec<RailType>>>();
    (trains, map)
  };
}

pub fn a() {
  let (mut trains, mut map) = DATA.clone();
  'outer: loop {
    trains.sort();
    for train in trains.iter_mut() {
      let success = train.ride(&mut map);
      if ! success {
        println!("{},{}",train.y,train.x);
        break 'outer;
      }
    }
  }
}

pub fn b() {
  let (mut trains, mut map) = DATA.clone();
  loop {
    trains.sort();
    let mut crashed : Vec<usize> = Vec::new();
    for i in 0..trains.len() {
      if let Some(_) = crashed.iter().position(|x| *x == i) { continue; }
      let success = trains.get_mut(i).unwrap().ride(&mut map);
      if ! success {
        crashed.push(i);
        for (j, other) in trains.iter().enumerate() {
          if j == i { continue; }
          if trains[i] == *other {
             crashed.push(j);
             break;
          }
        }
      }
    }
    //reverse order removal very important
    crashed.sort();
    crashed.reverse();
    for crash in crashed {
      trains.remove(crash);
    }
    if trains.len() == 1 {
      println!("{},{}",trains[0].y,trains[0].x);
      break;
    }
  }
}
