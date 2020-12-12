from enum import IntEnum

class DIR(IntEnum):
  NORTH=0
  EAST=1
  SOUTH=2
  WEST=3
  def turn(direction, degrees):
    i = int(degrees / 90)
    return DIR(( direction + i + 4 ) % 4)

class NavPC:
  def __init__(self):
    self.direction = DIR.EAST
    self.location = [0,0]
    self.instructions = {
      'L' : self.L,
      'R' : self.R,
      'N' : self.N,
      'S' : self.S,
      'E' : self.E,
      'W' : self.W,
      DIR.NORTH : self.N,
      DIR.SOUTH : self.S,
      DIR.EAST  : self.E,
      DIR.WEST  : self.W,
      'F' : self.F,
    }
  def L(self,  x):
    self.direction = DIR.turn(self.direction, -x)
  def R(self, x):
    self.direction = DIR.turn(self.direction, x)
  def N(self, x):
    self.location[0] = self.location[0] + x
  def S(self, x):
    self.location[0] = self.location[0] - x
  def E(self, x):
    self.location[1] = self.location[1] + x
  def W(self, x):
    self.location[1] = self.location[1] - x
  def F(self, x):
    self.instructions[self.direction](x),
      
  def run(self):
    for instruction in self.program:
      self.instructions[instruction[0]](instruction[1])

  def parse(self,filename):
    with open(filename,"r") as f:
      self.program = [ (line[0],int(line.rstrip('\n')[1:])) for line in f.readlines()]

pc = NavPC()
pc.parse('input')
pc.run()
print(sum([abs(x) for x in pc.location]))
