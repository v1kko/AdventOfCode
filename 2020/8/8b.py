import copy
import sys
with open("input", "r") as f:
  program = [[line.split()[0], int(line.rstrip('\n').split()[1])] for line in f.readlines() ]
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

  for i in range(len(program)):
    new_program = copy.deepcopy(program)
    regs['acc'] = 0
    regs['ip'] = 0
    if program[i][0] == 'jmp':
      new_program[i][0] = 'nop'
    elif program[i][0] == 'nop':
      new_program[i][0] = 'jmp'
    else:
      continue
  
    visited = []
    while True:
      visited.append(regs['ip'])
      rules[new_program[regs['ip']][0]](new_program[regs['ip']][1])
      if regs['ip'] in visited:
        break
      if regs['ip'] >= len(new_program):
        print(regs['acc'])
        sys.exit(0)
