import re
with open("input", "r") as f:
  correct = 0
  for line in f.readlines():
    m = re.match(r'(.*)-(.*) (.*): (.*)\n',line)
    lb,up,ch,pw = m.groups()
    lb = int(lb)-1
    up = int(up)-1
    if ( (pw[lb] == ch) != (pw[up] == ch) ) :
      correct = correct + 1
  print(correct)
