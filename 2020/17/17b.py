rawslice = [[0 if x == "." else 1 for x in list(line.rstrip("\n"))] for line in open("input","r").readlines()]

starti = len(rawslice)
startj = len(rawslice[0])

universe = {}

for i in range(starti):
  for j in range(len(rawslice[0])):
    universe[(i,j,0,0)] = rawslice[i][j]

for it in range(1,7):
  new_universe = {}
  for a in range(-it,it+starti+1):
   for b in range(-it,it+startj+1):
    for c in range(-it,it+1):
     for d in range(-it,it+1):
      try:
        x = universe[(a,b,c,d)]
      except:
        universe[(a,b,c,d)] = 0 
      val = universe[(a,b,c,d)]

      count = 0
      for i in range(-1,2):    
        for j in range(-1,2):    
          for k in range(-1,2):    
           for l in range(-1,2):    
            if i == j == k == l == 0:
              continue
            try:
              count += universe[(a+i,b+j,c+k, d+l)]
            except:
              pass
      if val == 1:
        if count == 2 or count == 3:
          new_universe[(a,b,c,d)] = 1
        else:
          new_universe[(a,b,c,d)] = 0
      else:
        if count == 3:
          new_universe[(a,b,c,d)] = 1
        else:
          new_universe[(a,b,c,d)] = 0
  universe = new_universe

print(sum(new_universe.values()))
