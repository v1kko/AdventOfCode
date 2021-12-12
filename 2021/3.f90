subroutine solver3(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer :: part, n, nn, in_l, g, e, det, oxy_n, co2_n 
  character(len=20) b_g, b_e, in_ls
  integer, allocatable :: occur(:) 
  type(char_p) :: input(:)
  logical              :: oxy(size(input)), co2(size(input))
  

  in_l = len(input(1)%p)
  write(in_ls, *) in_l

  if (part == 2) goto 2

  allocate(occur(in_l))
  occur = 0
  b_g = char(0)
  b_e = char(0)

  do n=1,size(input)
    do nn = 1, in_l
      if (input(n)%p(nn:nn) == "1") then
        occur(nn) = occur(nn) + 1
      endif
    enddo
  end do
  do n = 1, in_l
    if (occur(n) > size(input)/2) then
      b_g(n:n) = '1'
      b_e(n:n) = '0'
    else
      b_g(n:n) = '0'
      b_e(n:n) = '1'
    end if
  end do

  read(b_g,'(B'//in_ls//')') g
  read(b_e,'(B'//in_ls//')') e
  ans = g * e
  
  return

2 oxy = .true.
  co2 = .true.

3 do det = 1, in_l
    oxy_n = 0
    do n =1,size(input)
      if (oxy(n)) then
        if (input(n)%p(det:det) == "1") then
          oxy_n = oxy_n + 1
        end if
      end if
    end do
    if (oxy_n >= (count(oxy)+1)/2) then
      do n =1,size(input)
        if (input(n)%p(det:det) == "0") then
          oxy(n) = .false.
        end if
      end do
    else 
      do n =1,size(input)
        if (input(n)%p(det:det) == "1") then
          oxy(n) = .false.
        end if
      end do
    end if
    if (count(oxy) == 1)  exit
  end do
  if (count(oxy) > 1)  goto 3
  read(input(findloc(oxy,.true.,1))%p,'(B'//in_ls//')') oxy_n

4 do det = 1, in_l
    co2_n = 0
    do n =1,size(input)
      if (co2(n)) then
        if (input(n)%p(det:det) == "1") then
          co2_n = co2_n + 1
        end if
      end if
    end do
    if (co2_n < (count(co2)+1)/2) then
      do n =1,size(input)
        if (input(n)%p(det:det) == "0") then
          co2(n) = .false.
        end if
      end do
    else 
      do n =1,size(input)
        if (input(n)%p(det:det) == "1") then
          co2(n) = .false.
        end if
      end do
    end if
    if (count(co2) == 1)  exit
  end do
  if (count(co2) > 1)  goto 4
  read(input(findloc(co2,.true.,1))%p,'(B'//in_ls//')') co2_n

  ans = oxy_n * co2_n
  return
end subroutine
