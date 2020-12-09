code = [ int(line.rstrip('\n')) for line in open("input","r").readlines() ]


for i in range(25,len(code)):
  preamble = list(set(code[i-25:i]))
  word = code[i]
  for x in range(len(preamble)):
    for y in range(x+1,len(preamble)):
      if word == preamble[x]+preamble[y]:
        break
    else:
      continue
    break
  else:
    for j in range(i):
      count = 0
      for k in range(j,i):
        count += code[k]
        if count == word:
          solution = sorted(code[j:k+1])
          print(solution[0] + solution[-1])
