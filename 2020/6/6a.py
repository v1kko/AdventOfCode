with open("input","r") as f:
  groups = [ set(list("".join(group.split()))) for group in f.read().split('\n\n')]
  print (sum([len(group) for group in groups]))
