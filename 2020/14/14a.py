from collections import defaultdict
program = [tuple(line.rstrip('\n').split(" = ")) for line in open("input","r").readlines()]
mask = None
memory = defaultdict(lambda : list("0"*36))


for ins, arg in program:
  if ins == "mask":
    mask = list(arg)
  else:
    arg = list("{0:036b}".format(int(arg)))
    entry = int(ins.split(']')[0].split('[')[1])
    masked_arg = [ arg[i] if mask[i] == "X" else mask[i] for i in range(36)] 
    memory[entry] = int("".join(masked_arg),2)
print(sum(memory.values()))
