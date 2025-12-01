from enum import IntEnum
import numpy as np


class NavPC:
  def __init__(self):
    self.waypoint = [1,10]
    self.location = [0,0]
    self.instructions = {
      'L' : self.L,
      'R' : self.R,
      'N' : self.N,
      'S' : self.S,
      'E' : self.E,
      'W' : self.W,
      'F' : self.F,
    }
  def L(self,  x):
    wp = np.array(self.waypoint)
    c, s = int(round(np.cos(np.radians(-x)))), int(round(np.sin(np.radians(-x))))
    R = np.array(((c,-s),(s,c)))
    self.waypoint = (np.matmul(R,wp)).tolist()
  def R(self, x):
    wp = np.array(self.waypoint)
    c, s = int(round(np.cos(np.radians(x)))), int(round(np.sin(np.radians(x))))
    R = np.array(((c,-s),(s,c)))
    self.waypoint = np.matmul(R,wp).tolist()
  def N(self, x):
    self.waypoint[0] = self.waypoint[0] + x
  def S(self, x):
    self.waypoint[0] = self.waypoint[0] - x
  def E(self, x):
    self.waypoint[1] = self.waypoint[1] + x
  def W(self, x):
    self.waypoint[1] = self.waypoint[1] - x
  def F(self, x):
    self.location = [sum(y) for y in zip(self.location, [ x * c for c in self.waypoint])]

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
