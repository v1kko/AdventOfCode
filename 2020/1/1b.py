
with open("input","r") as f:
  nums = [int(x.rstrip('\n')) for x in f.readlines()]
  for i in range(len(nums)):
    for j in range(i,len(nums)):
      for k in range(j,len(nums)):
        if nums[i]+nums[j]+nums[k] == 2020:
          print(nums[i]*nums[j]*nums[k])
