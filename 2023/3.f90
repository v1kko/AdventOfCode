subroutine solver3(part,input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  integer :: part
  ans = 0
  select case(part)
  case(1)
    call solver_a(input,ans)
  case(2)
    call solver_b(input,ans)
  end select
contains
subroutine solver_a(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  integer :: n, m, s, d, i ,x(4)

  do n =1,size(input)
    p => input(n)%p
    d = 0
    do 
      s = scan(p(d+1:),'0123456789')
      if (s == 0) exit
      s = s + d
      d = verify(p(s:),'0123456789') + s - 2
      if (d == s - 2) then 
        d = len(p)
      end if
      read(p(s:d),*) i
      x = 0
      if (s > 1) then
        x(1) = verify(p(s-1:s-1),'.')
      end if
      if (d < len(p)) then
        x(2) = verify(p(d+1:d+1),'.')
      end if
      if ( n > 1 ) then
        x(3) = verify(input(n-1)%p(max(1,s-1):min(d+1,len(p))),'.')
      end if
      if (n < size(input)) then
        x(4) = verify(input(n+1)%p(max(1,s-1):min(d+1,len(p))),'.')
      end if
      do m = 1,4 
        if (x(m) /= 0) then
          ans = ans + i
          exit
        end if
      end do
    end do
  end do
end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  integer :: n, s, d, i ,g, x, y
  type gear
    integer i
  end type
  type(gear) :: gears(len(input(1)%p),size(input))
  gears = gear(0)
  y = 0
  x = 0

  do n =1,size(input)
    p => input(n)%p
    d = 0
    do 
      s = scan(p(d+1:),'0123456789')
      if (s == 0) exit
      s = s + d
      d = verify(p(s:),'0123456789') + s - 2
      if (d == s - 2) then 
        d = len(p)
      end if
      read(p(s:d),*) i
      if (s > 1) then
        g = scan(p(s-1:s-1),'*')
        if (g /= 0) then
          x = s-1
          y = n 
          goto 3
        end if
      end if
      if (d < len(p)) then
        g = scan(p(d+1:d+1),'*')
        if (g /= 0) then
          x = d+1
          y = n 
          goto 3
        end if
      end if
      if ( n > 1 ) then
        g = scan(input(n-1)%p(max(1,s-1):min(d+1,len(p))),'*')
        if (g /= 0) then
          x = g+max(1,s-1)-1
          y = n-1
          goto 3
        end if
      end if
      if (n < size(input)) then
        g = scan(input(n+1)%p(max(1,s-1):min(d+1,len(p))),'*')
        if (g /= 0) then
          x = g+max(1,s-1)-1
          y = n+1
          goto 3
        end if
      end if
      cycle
3     continue
      if ( gears(x,y)%i /= 0 ) then
        ans = ans + gears(x,y)%i*i
      else
        gears(x,y)%i=i
      end if
    end do
  end do
end subroutine
end subroutine
