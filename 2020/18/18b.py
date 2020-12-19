raweqs = [line.rstrip('\n').split() for line in open("input","r").readlines()]

def parse_eq(raweq):
  eq = []
  for x in raweq:
    if "(" in x:
      eq = eq + ["(" for _ in range(x.count("("))] + [int(x[x.count("("):])]
    elif ")" in x:
      eq = eq + [int(x[:-x.count(")")])] + [")" for _ in range(x.count(")"))]
    elif x == "*" or x == "+":
      eq.append(x)
    else:
      eq.append(int(x))
  return eq

def stack(eq, it=0):
  res = []
  while True:
    val = eq[it]
    if val == ")":
      return res, it
    if val == "(":
      x, it = stack(eq,it+1)
      res.append(x)
    else:
      res.append(val)
    it += 1
    if it == len(eq):
      return res

import math
def solve(eq):
  if isinstance(eq, int):
    return eq
  if isinstance(eq[0], list):
    neweq = [solve(eq[0])]
  else:
    neweq = [eq[0]]
  for i in range(1,len(eq),2):
    if eq[i] == "+":
      neweq[-1] = neweq[-1] + solve(eq[i+1])
    else:
      neweq.append(solve(eq[i+1]))
  return math.prod(neweq)
      

print(sum([solve(stack(parse_eq(eq))) for eq in raweqs]))
