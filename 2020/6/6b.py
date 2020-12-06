with open("input","r") as f:
  groups = [[list(person) for person in  group.split()] for group in f.read().split('\n\n')]
  count = 0
  for group in groups:
    all_set = group[0]
    for person in group:
      new_set = []
      for x in all_set:
        if x in person:
          new_set.append(x)
      all_set = new_set
    count += len(all_set)
  print(count)
