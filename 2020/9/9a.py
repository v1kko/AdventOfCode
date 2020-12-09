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
    print(preamble, word)
