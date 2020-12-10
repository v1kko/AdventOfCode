adp =  sorted( [ int(line.rstrip('\n')) for line in open("input","r").readlines() ] )
adp = [0] + adp + [adp[-1]+3]
diffs = [adp[i] - adp[i-1] for i in range(1,len(adp))] 
total = 1
cur = 0
for diff in diffs:
  if diff == 3:
    if cur == 1 or cur == 0:
      pass
    elif cur == 2:
      total *= 2
    elif cur == 3:
      total *= 4
    elif cur == 4:
      total *= 7
    else:
      print("error")
    cur = 0
  else:
    cur += 1
print ( total )
