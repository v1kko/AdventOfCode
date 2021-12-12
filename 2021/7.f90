subroutine solver7(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, n_crab, max_crab, min_fuel, fuel
  integer, allocatable :: crabs(:)
  type(char_p) :: input(:)

  n_crab = 1
  do n = 1, len(input(1)%p)
    if (input(1)%p(n:n) == ',') n_crab = n_crab +1
  end do
  allocate(crabs(n_crab))
  read(input(1)%p,*) crabs

  max_crab = maxval(crabs)

  if (part == 2) goto 2

  min_fuel = sum(crabs)

  do n = 1,max_crab
    fuel = sum(abs(crabs - n))
    if (fuel < min_fuel) then
      min_fuel = fuel
    end if
  end do
  ans = min_fuel
  deallocate(crabs)
  return

2 min_fuel = sum((crabs)*(crabs+1)/2)
  do n = 1,max_crab
    fuel = sum(abs(crabs - n)*(abs(crabs -n)+1)/2)
    if (fuel < min_fuel) then
      min_fuel = fuel
    end if
  end do
  ans = min_fuel
  deallocate(crabs)
  return
  
end subroutine
