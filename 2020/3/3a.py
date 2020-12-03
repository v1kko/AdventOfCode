with open("input", "r") as f:
  grid = [[0 if y == "." else 1 for y in x.rstrip("\n")] for x in f.readlines()]
  y = len(grid[0])
  trees = 0
  yy = 0
  for i in range(len(grid)):
    trees = trees + grid[i][yy]
    yy = (yy + 3 ) % y
  
  print (trees)  
