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

def solve(eq, it=0):
  if eq[it] == "(":
    ans, it = solve(eq,it+1)
  else:
    ans = eq[it]
  while True:
    it += 1
    op = eq[it]
    if op == ")":
      return ans, it
    it += 1
    ans2 = eq[it] 
    if ans2 == "(":
      ans2, it = solve(eq,it+1)
    if op == "*":
      ans = ans * ans2
    if op == "+":
      ans = ans + ans2
    if it == len(eq)-1:
      return ans, it

eqs = [ parse_eq(raweq) for raweq in raweqs]
print(sum([solve(eq)[0] for eq in eqs]))
