with open("input", "r") as f:
  program = [(line.split()[0], int(line.rstrip('\n').split()[1])) for line in f.readlines() ]
  regs = { 'acc' : 0, 'ip' : 0 }

  def rule_acc(x):
    regs['acc'] += x
    regs['ip'] += 1
  def rule_nop(x):
    regs['ip'] = regs['ip'] + 1
  def rule_jmp(x):
    regs['ip'] = regs['ip'] + x

  rules = {
    'nop' : rule_nop,
    'acc' : rule_acc,
    'jmp' : rule_jmp,
  }

  visited = []
  while True:
    visited.append(regs['ip'])
    rules[program[regs['ip']][0]](program[regs['ip']][1])
    if regs['ip'] in visited:
      break
  print (regs['acc'])
    

