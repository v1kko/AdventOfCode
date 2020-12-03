import math
with open("input", "r") as f:
  grid = [[0 if y == "." else 1 for y in x.rstrip("\n")] for x in f.readlines()]
  y = len(grid[0])
  trees = [0,0,0,0,0]
  yy = 0
  for i in range(len(grid)):
    trees[0] = trees[0] + grid[i][yy]
    yy = (yy + 1 ) % y
  yy = 0
  for i in range(len(grid)):
    trees[1] = trees[1] + grid[i][yy]
    yy = (yy + 3 ) % y
  yy = 0
  for i in range(len(grid)):
    trees[2] = trees[2] + grid[i][yy]
    yy = (yy + 5 ) % y
  yy = 0
  for i in range(len(grid)):
    trees[3] = trees[3] + grid[i][yy]
    yy = (yy + 7 ) % y
  yy = 0
  for i in range(len(grid)):
    if i % 2 == 0 :
      trees[4] = trees[4] + grid[i][yy]
      yy = (yy + 1 ) % y
  
  print (trees)
  print (math.prod(trees))  
