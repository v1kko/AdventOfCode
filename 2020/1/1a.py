
with open("input","r") as f:
  nums = [int(x.rstrip('\n')) for x in f.readlines()]
  for i in range(len(nums)):
    for j in range(i,len(nums)):
      if nums[i]+nums[j] == 2020:
        print(nums[i]*nums[j])
