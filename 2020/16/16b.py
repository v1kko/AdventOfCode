rules = {}
mine = []
othertickets = []

def invalid_fields(ticket):
  ret = []
  for val in ticket:
    for key, rule in rules.items():
      if check_rule(val, rule):
        break
    else:
      ret.append(val)
  return ret

def check_rule(val, limits):
  if ( limits[0] <= val <= limits[1] ) or ( limits[2] <= val <= limits[3] ):
    return True
  return False

with open("input","r") as f:
  rawlines = f.read()
  rawrules, rawmyticket, rawothertickets = rawlines.split('\n\n')
  rules_l = rawrules.split("\n")
  for rule in rules_l:
    key, rest = rule.split(":")
    _, a, _, b = rest.split(" ")
    rules[key] = [ int(x) for x in a.split("-") + b.split("-")]
  mine = rawmyticket.split(",")
  othertickets = [ [ int(x) for x in otherticket.split(",")] for otherticket in rawothertickets.rstrip('\n').split("\n")[1:] ]

othertickets = [ticket for ticket in othertickets if len(invalid_fields(ticket)) == 0]

rulemap = {}

for key, val in rules.items():
  rulemap[key] = []
  for i in range(len(othertickets[0])):
    for ticket in othertickets:
      if not check_rule(ticket[i],val):
        break
    else:
      rulemap[key].append(i)

def find_permutation(vals, taken):
  if len(vals) == 0:
    return {}
  key, poss = vals[0]
  for i in poss:
    if i not in taken:
      newtaken = taken + [i]
      newdict = find_permutation(vals[1:],newtaken)
      if newdict != None:
        newdict[key] = i
        return newdict
  return None

rulemap = find_permutation(list(rulemap.items()),[])


import math
print(math.prod([int(mine[val]) for key, val in rulemap.items() if "departure" in key]))
