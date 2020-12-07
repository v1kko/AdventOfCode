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
      val.append((nam + " " + nam2, int(num)))
    rules[p] = val

  contains = lambda x : 1 + sum([ num * contains(bag) for bag, num in rules[x] ])
  print(contains('shiny gold')-1)

