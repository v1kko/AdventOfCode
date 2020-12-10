from collections import Counter
adp =  sorted( [ int(line.rstrip('\n')) for line in open("input","r").readlines() ] )
adp = [0] + adp + [adp[-1]+3]
diffs = [adp[i] - adp[i-1] for i in range(1,len(adp))] 
counts = Counter(diffs)
print(counts[1]*counts[3])
