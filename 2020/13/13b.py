from math import lcm
def extended_gcd(a, b):
    """Extended Greatest Common Divisor Algorithm

    Returns:
        gcd: The greatest common divisor of a and b.
        s, t: Coefficients such that s*a + t*b = gcd

    Reference:
        https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
    """
    old_r, r = a, b
    old_s, s = 1, 0
    old_t, t = 0, 1
    while r:
        quotient, remainder = divmod(old_r, r)
        old_r, r = r, remainder
        old_s, s = s, old_s - quotient * s
        old_t, t = t, old_t - quotient * t

    return old_r, old_s, old_t
def combine_phased_rotations(a_period, a_phase, b_period, b_phase):
    """Combine two phased rotations into a single phased rotation

    Returns: combined_period, combined_phase

    The combined rotation is at its reference point if and only if both a and b
    are at their reference points.
    """
    gcd, s, t = extended_gcd(a_period, b_period)
    phase_difference = a_phase - b_phase
    pd_mult, pd_remainder = divmod(phase_difference, gcd)
    if pd_remainder:
        raise ValueError("Rotation reference points never synchronize.")

    combined_period = a_period // gcd * b_period
    combined_phase = (a_phase - s * pd_mult * a_period) % combined_period
    return combined_period, combined_phase

busses = [0 if bus == 'x' else int(bus) for bus in open("input","r").readlines()[1].rstrip('\n').split(',')]

offset = 0
period = busses[0]
periods = [busses[0]]
offsets = [0]

for i in range(1,len(busses)):
  bus = busses[i]
  if bus == 0:
    continue
  x = 0
  while True:
    if  not ( ( x + i ) % bus ) :
        periods.append(lcm(period,bus))
        offsets.append(x)
        break
    x += busses[0]

cur_off = offsets[0]
cur_per = periods[0]

for i in range(1,len(offsets)):
  cur_per, cur_off = combine_phased_rotations(periods[i],offsets[i],cur_per,cur_off)
  
print(cur_off)
