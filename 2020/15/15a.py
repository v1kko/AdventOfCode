it = 0
start = [ int(x) for x in open("input","r").readline().rstrip('\n').split(',')]

hist = {}
cur = start[0]
last = start[0]
for n in start[1:]:
  hist[last] = it
  last = n
  it += 1  
  
while it < 2019:
  if last in hist:
    tmp  = hist[last]
    hist[last] = it
    last = it - tmp
  else:
    hist[last] = it
    last = 0

  it += 1

print(last)
