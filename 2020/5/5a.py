print(
  max(
    [  
      sum([2**i for i in range(7) if list(ticket[:7])[-i -1] == 'B']) * 8
    + sum([2**i for i in range(3) if list(ticket[7:-1])[-i -1] == 'R'])
      for ticket in open("input", "r").readlines()
    ]
  )
)
