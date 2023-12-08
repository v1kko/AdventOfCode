subroutine solver8(part,input,ans)
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
  integer :: n, m
  logical :: instructions(len(input(1)%p))
  character(len=3) :: map(size(input)-2), aleft, aright
  integer :: graph(2, size(input)-2)
  integer :: cur = -1, stop = -1

  do n = 1, len(input(1)%p)
    select case(input(1)%p(n:n))
    case('L')
      instructions(n) = .true.
    case('R')
      instructions(n) = .false.
    end select
  end do

  do n = 3, size(input)
    map(n-2) = input(n)%p(1:3)
    if (map(n-2) == "AAA") cur = n-2
    if (map(n-2) == "ZZZ") stop = n-2
  end do

  do n = 3,size(input)
    p => input(n)%p
    aleft = p(8:10)
    aright = p(13:15)
    do m = 1, size(map)
      if (map(m) == aleft) graph(1,n-2) = m
      if (map(m) == aright) graph(2,n-2) = m
    end do
  end do
  n = 0
  do while(cur /= stop)
    ans = ans + 1
    n = mod(n,size(instructions)) + 1
    if (instructions(n)) then
      cur = graph(1,cur)
    else
      cur = graph(2,cur)
    end if
  end do
end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  integer :: n, m, l, x
  logical :: instructions(len(input(1)%p))
  character(len=3) :: map(size(input)-2), aleft, aright
  integer :: graph(2, size(input)-2)
  integer :: curs(size(input)), starts(size(input)), stops(size(input))
  integer(int64) :: periodicity(size(input))
  integer :: ncurs,nstops
  ncurs = 0
  nstops = 0

  do n = 1, len(input(1)%p)
    select case(input(1)%p(n:n))
    case('L')
      instructions(n) = .true.
    case('R')
      instructions(n) = .false.
    end select
  end do

  do n = 3, size(input)
    map(n-2) = input(n)%p(1:3)
    if (map(n-2)(3:3) == "A") then
      ncurs = ncurs + 1
      curs(ncurs) = n-2
    end if
    if (map(n-2)(3:3) == "Z") then
      nstops = nstops +1
      stops(nstops)= n-2
    end if
  end do

  do n = 3,size(input)
    p => input(n)%p
    aleft = p(8:10)
    aright = p(13:15)
    do m = 1, size(map)
      if (map(m) == aleft) graph(1,n-2) = m
      if (map(m) == aright) graph(2,n-2) = m
    end do
  end do

  starts = curs

  do m=1,ncurs
    n = 0
    l = 0
  outer: do 
      l = l + 1
      n = mod(n,size(instructions)) + 1
      if (instructions(n)) then
          curs(m) = graph(1,curs(m))
      else
          curs(m) = graph(2,curs(m))
      end if
      do x = 1, nstops
        if (curs(m) == stops(x)) then
          periodicity(m) = l
          exit outer
        end if
      end do
    end do outer
  end do
  ans = 1
  do n = 1,ncurs
    ans = lcm(ans, periodicity(n))
  end do
end subroutine
function gcd(a, b)
  integer(int64) :: a, b, gcd, r
  do  while (b/=0)
    r=mod(a,b)
    a=b 
    b=r 
  end do 
  gcd = a
end function
function lcm(a,b)
  integer(int64) :: a,b,lcm
  lcm = (a*b)/gcd(a,b)
end function
end subroutine
