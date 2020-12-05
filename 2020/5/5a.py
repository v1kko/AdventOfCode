with open("input", "r") as f:
  tickets = f.readlines()
  seat_ids = []
  for ticket in tickets:
    row_raw = list(ticket[:7])
    row = sum([2**i for i in range(7) if row_raw[-i -1] == 'B'])
    seat_raw = list(ticket[7:-1])
    seat= sum([2**i for i in range(3) if seat_raw[-i -1] == 'R'])
    seat_ids.append(seat + row*8)
  print(max(seat_ids))
