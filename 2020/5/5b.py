with open("input", "r") as f:
  tickets = f.readlines()
  seat_ids = []
  for ticket in tickets:
    row_raw = list(ticket[:7])
    row = sum([2**i for i in range(7) if row_raw[-i -1] == 'B'])
    seat_raw = list(ticket[7:-1])
    seat= sum([2**i for i in range(3) if seat_raw[-i -1] == 'R'])
    seat_ids.append(seat + row*8)
  seat_ids = sorted(seat_ids)
  for i in range(1,len(seat_ids)):
    if seat_ids[i-1] != seat_ids[i] -1:
      print(seat_ids[i-1:i+1],sum(seat_ids[i-1:i+1])/2)
