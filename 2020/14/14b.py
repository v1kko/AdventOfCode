from collections import defaultdict
from copy import deepcopy as dp
program = [tuple(line.rstrip('\n').split(" = ")) for line in open("input","r").readlines()]
mask = None
memory = defaultdict(lambda : list("0"*36))

def memadd(addr, n, value, mask, entry):
  if n == 36:
    memory[int("".join(addr),2)] = value
    return

  if mask[n] == "0":
    addr.append(entry[n])
    memadd(addr, n+1, value, mask, entry)
    return
  elif mask[n] == "1":
    addr.append("1")
    memadd(addr, n+1, value, mask, entry)
    return
  else:
    new_addr = dp(addr)
    new_addr.append("1")
    memadd(new_addr, n+1, value, mask, entry)
    addr.append("0")
    memadd(addr, n+1, value, mask, entry)
    return

for ins, arg in program:
  if ins == "mask":
    mask = list(arg)
  else:
    arg = int(arg)
    entry = list("{0:036b}".format(int(ins.split(']')[0].split('[')[1])))
    memadd([],0,arg,mask,entry)

print(sum(memory.values()))
