from copy import deepcopy as dp
import numpy as np
area = [
    [ 0 if x == "." else -1 if x == "L" else 2 for x in list(line.rstrip('\n'))]
    for line in open("input","r").readlines()
  ]
while True:
  new_area = dp(area)
  for i in range(len(area)):
    for j in range(len(area[0])):
      if area[i][j] == 0:
        continue
      elif area[i][j] == -1:
        for ii in range(max(0,i-1),min(len(area),i+2)):
          for jj in range(max(0,j-1),min(len(area[0]),j+2)):
            if ii == i and j == jj:
              continue
            if area[ii][jj] == 1:
              new_area[i][j] = -1
              break
          else:
            continue
          break
        else:
          new_area[i][j] = 1
      elif area[i][j] == 1:
        count = 0
        for ii in range(max(0,i-1),min(len(area),i+2)):
          for jj in range(max(0,j-1),min(len(area[0]),j+2)):
            if ii == i and j == jj:
              continue
            if area[ii][jj] == 1:
              count += 1
              if count == 4:
                new_area[i][j] = -1
                break
              continue
          else:
            continue
          break
        else:
          continue
  for i in range(len(area)):
    for j in range(len(area[0])):
      if area[i][j] == new_area[i][j]:
        continue
      break
    else:
      continue
    break
  else:
    break

  area = dp(new_area)
print(sum([sum([ x for x in line if x == 1]) for line in new_area]))
