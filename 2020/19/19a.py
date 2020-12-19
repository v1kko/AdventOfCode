rawrules, rawimages = open("input","r").read().split('\n\n')
rawrules = rawrules.split('\n')
images = rawimages.split('\n')[:-1]
rules = {}
for rawrule in rawrules:
  rule, val = rawrule.split(":")
  val = val.lstrip(' ')
  vals = val.split("|")
  vals = [val.strip(' ').strip('"').split(' ') for val in vals]
  rules[rule] = vals

def valids(rule):
  if len(rules[rule]) == 1 and len(rules[rule][0]) == 1 and rules[rule][0][0].isalpha() :
    return rules[rule]
    
  ret = []
  for split in rules[rule]:
    x = [ valids(child) for child in split ]
    if len(x) == 1:
      ret = ret + x[0]
    elif len(x) == 2:
      for i in range(len(x[0])):
        for j in range(len(x[1])):
          ret.append(x[0][i] + x[1][j])
    elif len(x) == 3:
      for i in range(len(x[0])):
        for j in range(len(x[1])):
          for k in range(len(x[2])):
            ret.append(x[0][i] + x[1][j] + x[2][k])
  return ret
      
valids_str = [ "".join(valid) for valid in valids('0')]
    
print(len( [image for image in images if image in valids_str]))
