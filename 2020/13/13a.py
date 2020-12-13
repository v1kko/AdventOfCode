with open("input","r") as f:
  time = int ( f.readline().rstrip('\n') )
  busses = [int(bus) for bus in f.readline().rstrip('\n').split(',') if bus != 'x']

  fast = busses[0]
  wait = fast-time%fast

  for bus in busses:
    if wait > bus-time%bus:
      fast = bus
      wait = bus-time%bus
  print(fast,wait,fast*wait)
