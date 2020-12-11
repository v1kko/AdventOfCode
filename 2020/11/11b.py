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
        for ii in range(-1,2):
          for jj in range(-1,2):
            if ii == 0 and jj == 0:
              continue
            iii = i
            jjj = j
            while True:
              iii = iii + ii
              jjj = jjj + jj
              if (iii < 0) or( iii  >= len(area)) or( jjj  < 0 )or( jjj  >= len(area[0])) :
                break
              if area[iii][jjj] != 0:
                break
            if (iii < 0) or( iii  >= len(area)) or( jjj  < 0 )or( jjj  >= len(area[0])) :
              continue
            if area[iii][jjj] == 1:
              break
          else:
            continue
          break
        else:
          new_area[i][j] = 1
      elif area[i][j] == 1:
        count = 0
        for ii in range(-1,2):
          for jj in range(-1,2):
            if ii == 0 and jj == 0:
              continue
            iii = i
            jjj = j
            while True:
              iii = iii + ii
              jjj = jjj + jj
              if (iii < 0) or( iii  >= len(area)) or( jjj  < 0 )or( jjj  >= len(area[0])) :
                break
              if area[iii][jjj] != 0:
                break
            if (iii < 0) or( iii  >= len(area)) or( jjj  < 0 )or( jjj  >= len(area[0])) :
              continue
            if area[iii][jjj] == 1:
              count += 1
              if count == 5:
                new_area[i][j] = -1      
                break
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
print(sum([sum([ 1 for x in line if x == 1]) for line in new_area]))
