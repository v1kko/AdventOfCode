subroutine solver8(part,input,ans)
  use iso_c_binding
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, d_l, nn, x
  type(char_p) :: input(:)
  character(len=8), target :: digit(4), wires(10), dum
  character(len=8) :: map(0:9)
  integer :: res
  ans = 0

  if (part == 2) goto 2

  do n=1,size(input)
    read(input(n)%p,*) wires, dum, digit
    do nn = 1, 4
      d_l = len(trim(digit(nn)))  
      if ((d_l .eq. 2) .or. (d_l .eq. 3) .or. (d_l .eq. 4) .or. (d_l .eq. 7)) then
        ans = ans + 1  
      end if
    end do
  end do
  return

2 continue

  do n=1,size(input)
    read(input(n)%p,*) wires, dum, digit
    do nn = 1, 10
      call qsort(c_loc(wires(nn)),int(len(trim(wires(nn))),int64) &
                ,int(len(' '),int64),c_funloc(compar))
    end do
    do nn = 1, 4
      call qsort(c_loc(digit(nn)),int(len(trim(digit(nn))),int64) &
                , int(len(' '),int64),c_funloc(compar))
    end do
    
    do nn = 1, 10
      if (len(trim(wires(nn))) == 2)  map(1) = wires(nn)
      if (len(trim(wires(nn))) == 3)  map(7) = wires(nn)
      if (len(trim(wires(nn))) == 4)  map(4) = wires(nn)
      if (len(trim(wires(nn))) == 7)  map(8) = wires(nn)
    end do
    do nn = 1, 10
      if (len(trim(wires(nn))) == 5) then
        do x = 1, 3
          if (0 == index(wires(nn),map(7)(x:x))) goto 3 
        end do
        map(3) = wires(nn)
3     end if
    end do
    do nn = 1, 10
      if (len(trim(wires(nn))) == 6) then
        do x = 1, 5
          if (0 == index(wires(nn),map(3)(x:x))) goto 4
        end do
        map(9) = wires(nn)
4     end if
    end do
    do nn = 1, 10
      if (len(trim(wires(nn))) == 5) then
        if (wires(nn) == map(3)) cycle
        do x = 1, 5
          if (0 == index(map(9),wires(nn)(x:x))) goto 5
        end do
        map(5) = wires(nn)
5     end if
    end do
    do nn = 1, 10
      if (len(trim(wires(nn))) == 5) then
        if (wires(nn) == map(3)) cycle
        if (wires(nn) == map(5)) cycle
        map(2) = wires(nn)
      end if  
    end do
    do nn = 1, 10
      if (len(trim(wires(nn))) == 6) then
        if (wires(nn) == map(9)) cycle
        do x = 1, 5
          if (0 == index(wires(nn),map(5)(x:x))) goto 6
        end do
        map(6) = wires(nn)
6     end if  
    end do
    do nn = 1, 10
      if (len(trim(wires(nn))) == 6) then
        if (wires(nn) == map(9)) cycle
        if (wires(nn) == map(6)) cycle
        map(0) = wires(nn)
      end if  
    end do
    
    res = 0
    do nn = 1,4
      do x = 0,9
        if (digit(nn) == map(x)) then
          res = res + x*10**(4-nn)
        end if
      end do
    end do
    ans = ans + res
  end do

contains
function compar(a,b)
  integer*1 :: a,b
  integer*2 :: compar
  if ( a .lt. b ) compar = -1
  if ( a .eq. b ) compar = 0
  if ( a .gt. b ) compar = 1
end function
end subroutine
