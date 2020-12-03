import re
with open("input", "r") as f:
  correct = 0
  for line in f.readlines():
    m = re.match(r'(.*)-(.*) (.*): (.*)\n',line)
    lb = int(m.group(1))
    up = int(m.group(2))
    ch = m.group(3)
    pw = m.group(4)
    if ( ( lb <= pw.count(ch) <= up ) ) :
      correct = correct + 1
  print(correct)
