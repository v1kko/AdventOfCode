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

print(sum([sum(invalid_fields(ticket)) for ticket in othertickets]))
  
