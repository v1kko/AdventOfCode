import copy

with open("input","r") as f:
  raw_rules = f.readlines()
  rules = {}
  for raw_rule in raw_rules:
    p, c = raw_rule.rstrip('\n').split(' bags contain ')
    val = []
    for bag in c.split(","):
      bag = bag.rstrip(' ').rstrip('.').rstrip('bags').rstrip('bag').rstrip(' ')
      bag = bag.lstrip(' ')
      if (bag == 'no other'):
        continue
      num, nam, nam2 = bag.split(' ')
      val.append((nam + " " + nam2, num))
    rules[p] = val

  visited = ['shiny gold']
  new_visited = copy.deepcopy(visited)
  while True:
    for key, val in rules.items():
      if key in visited:
        continue
      for bag, num in val:
        if bag in visited:
          new_visited.append(key)
          break
    if len(new_visited) == len(visited):
      break
    visited = copy.deepcopy(new_visited)
  print(len(visited)-1)
