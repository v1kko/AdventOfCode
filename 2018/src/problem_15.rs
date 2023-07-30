use std::fs;
use std::fmt;
use std::cmp::Ordering;
use lazy_static::lazy_static;

#[derive(Clone, Debug, PartialEq)]
enum Tile {
  Wall,
  Floor,
  Elf{id : usize},
  Goblin{id : usize},
}

#[derive(Clone, Debug, Copy, PartialEq)]
enum Orientation {
  Empty,
  North,
  East,
  South,
  West, 
  Visited, 
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum Species {
  Elf,
  Goblin
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Player {
  health : i32,
  x : usize,
  y : usize,
  dead : bool,
  spec: Species,
}

#[derive(Clone, Debug)]
struct Map(Vec<Vec<Tile>>);

impl fmt::Display for Map {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let map = &self.0;
    for line in map.iter() {
      for tile in line.iter() {
        let c : char =  match tile {
            Tile::Floor => '.',
            Tile::Wall => '#',
            Tile::Goblin{..} => 'G',
            Tile::Elf{..} => 'E',
        };
        write!(f, "{}", c)?;
      }
      writeln!(f)?;
    }
    Ok(())
  }
}

impl Tile {
  fn is_enemy(&self, other :&Self) -> bool {
    match self {
      Tile::Elf{..} => match other { Tile::Goblin{..} => true, _ => false },
      Tile::Goblin{..} => match other { Tile::Elf{..} => true, _ => false },
      _ => false }
  }
}

impl PartialOrd for Player {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      Some(self.cmp(other))
  }
}
impl Ord for Player {
  fn cmp(&self, other: &Self) -> Ordering {
    if self.y == other.y {
      self.x.cmp(&other.x)
    } else {
      self.y.cmp(&other.y)
    }
  }
}



lazy_static! {
  static ref DATA : (Map, Vec<Player>)  = { 
    let mut players : Vec<Player> = vec![];
    let txt         = fs::read_to_string("15.txt").unwrap(); 
    let map = txt[..txt.len()-1].split('\n').enumerate().map(|(y,line)| {
      line.chars().enumerate().map(|(x,c)| {
        match c {
          '#' => Tile::Wall,
          '.' => Tile::Floor,
          'G' => {
            players.push(Player{health:200, x:x,y:y, dead:false, spec:Species::Goblin});
            Tile::Goblin{id : players.len()-1}
          },
          'E' => {
            players.push(Player{health:200,x:x,y:y, dead:false, spec:Species::Elf});
            Tile::Elf{id : players.len()-1}
          },
          _ => panic!("Unexpected Letter: {}",c)
        }
      }).collect::<Vec<Tile>>()
    }).collect::<Vec<Vec<Tile>>>();
    (Map(map), players)
  };
}

type Position = (i32,i32);

static DIRECTIONS : [(Position, Orientation);4] = [((-1,0),Orientation::North),
                                                     ((0,-1),Orientation::West),
                                                     ((0,1),Orientation::East),
                                                     ((1,0),Orientation::South)];

impl Player {
  fn attack(&self, map: &mut Map, players : &mut Vec<Player>, n_elves: &mut usize, n_goblins: &mut usize, elf_power: i32) {
    let mut enemies : Vec<(usize, usize, i32, usize)> = vec![];
    for ((py,px), _) in DIRECTIONS.iter() {
      let x = (self.x as i32 + px) as usize;
      let y = (self.y as i32 + py) as usize;
      let me = &map.0[self.y][self.x];
      if  me.is_enemy(&map.0[y][x]) {
        let enemy = match map.0[y][x] {
          Tile::Goblin{id} | Tile::Elf{id} => &players[id],
          _ => {panic!("no enemy found to hit!");}
        };
        enemies.push((y,x,enemy.health, enemies.len()))
      }
    }
    if enemies.len() == 0 { return; }
    enemies.sort_by(|a,b| 
                    if  a.2.cmp(&b.2) == Ordering::Equal {
                      a.3.cmp(&b.3)
                    } else { 
                      a.2.cmp(&b.2)
                    });
    
    let (y,x,_,_) = enemies[0];
    let enemy = match map.0[y][x] {
      Tile::Goblin{id} | Tile::Elf{id} => &mut players[id],
      _ => {panic!("no enemy found to hit!");}
    };
    match enemy.spec {
      Species::Elf => {enemy.health -= 3;},
      Species::Goblin => {enemy.health -= elf_power;},
    };
    if enemy.health <= 0 {
      match enemy.spec {
        Species::Elf => {*n_elves -= 1;},
        Species::Goblin => {*n_goblins -= 1;},
      };
      enemy.dead = true;
      enemy.health = 0;
      map.0[y][x] = Tile::Floor
    }
  }

  fn step(&mut self, map: &mut Map) {
    let x = self.x;
    let y = self.y;
    let p_map = &map.0[y][x];

    let mut dist_map : Vec<Vec<Orientation>> = vec![vec![Orientation::Empty;map.0[0].len()];map.0.len()];
    let mut cur : Vec<Position> = vec![(y as i32,x as i32)];
    dist_map[y][x] = Orientation::Visited;
    loop {
      let mut newcur : Vec<Position> = vec![];
      let mut enemies : Vec<(Orientation, usize, usize)> = vec![];
      for (cy,cx) in cur.iter() {
        for ((py,px), orientation) in DIRECTIONS.iter() {
          let newx :usize =( cx+px) as usize;
          let newy:usize = (cy+py) as usize;
          if dist_map[newy][newx] !=  Orientation::Empty { continue }; 
          let orientation : Orientation = match dist_map[*cy as usize][*cx as usize] {
            Orientation::Visited => orientation.clone(),
            Orientation::Empty => orientation.clone(),
            _ => dist_map[*cy as usize][*cx as usize]
          };
          if p_map.is_enemy(&map.0[newy][newx]) { 
            if *cy != y as i32  || *cx != x as i32 {
              enemies.push((orientation,newy,newx));
            } else {
              return;
            }
          }
          if let Tile::Floor = map.0[newy][newx] {
            newcur.push((newy as i32,newx as i32));
            dist_map[newy][newx] = orientation; 
          }
        }
      }
      if enemies.len() != 0 {
        enemies.sort_by(|a,b| if a.1.cmp(&b.1) == Ordering::Equal { a.2.cmp(&b.2) } else { a.1.cmp(&b.1) });

        let (orientation, _, _) = enemies[0];
        
        let (ny, nx) = match orientation {
          Orientation::North => (-1,0),
          Orientation::West => ( 0,-1),
          Orientation::East => (0,1),
          Orientation::South => (1,0),
          _ => { panic!("this should not happen");}};

        self.x = (self.x as i32 + nx) as usize;
        self.y = (self.y as i32 + ny) as usize;
        map.0[self.y][self.x] = p_map.clone();
        map.0[y][x] = Tile::Floor;
        return;
      }
      if newcur.len() == 0 {
        return;
      }
      cur = newcur;
    }
  }
}


pub fn a() {
  let answer = simulate(3, true).unwrap();
  println!("{}", answer);
}

fn simulate(elf_power : i32, allow_dead_elves : bool) -> Option<i32> {
    let (mut map,mut players) = DATA.clone();
    let mut cnt = 0;
    let mut n_elves = players.iter().filter(|player| player.spec == Species::Elf).count();
    let start_n_elves = n_elves;
    let mut n_goblins = players.iter().filter(|player| player.spec == Species::Goblin).count();
    'outer: loop {
      let mut player_order : Vec<(usize,Player)> = players.clone().into_iter().enumerate().collect();
      player_order.sort_by(|a,b| a.1.cmp(&b.1));
      let player_order = player_order.into_iter().map(|(i,_)|i);

      for i in player_order {
        if players[i].dead { continue; }
        if n_goblins == 0 || n_elves == 0 {
          break 'outer;
        }
        players[i].step(&mut map);
        players[i].clone().attack(&mut map, &mut players, &mut n_elves, &mut n_goblins,elf_power);
        if start_n_elves != n_elves && ! allow_dead_elves { return None }
      }
      cnt +=1;
    }
    let health : i32 =  players.into_iter().map(|player| player.health).sum();
    return Some(health*cnt);
}

pub fn b() {
  let mut elf_power = 3;
  loop {
    elf_power += 1;
    if let Some(answer) = simulate(elf_power, false) {
      println!("{}", answer);
      break;
    }
  }
}
