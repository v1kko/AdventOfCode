subroutine solver9(part,input,ans)
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
  integer :: n, x, y
  integer, allocatable :: seq(:)
  x = 1
  n = 1
  do
    y = scan(input(1)%p(x:), ' ')
    if (y == 0) exit
    x = x + y
    n = n + 1
  end do
  allocate(seq(n))

  do n =1,size(input)
    p => input(n)%p
    read(p,*) seq
    ans = ans + extrapolate_a(seq)
  end do
end subroutine
recursive function extrapolate_a(seq) result(x)
  implicit none
  integer :: x
  integer :: seq(:)
  integer, allocatable :: interpolate_a(:)
  integer n

  if (all(seq==0)) then
    x = 0
    return
  end if
  allocate(interpolate_a(size(seq)-1))
  do n = 1, size(interpolate_a)
    interpolate_a(n) = seq(n+1) - seq(n)
  end do
  x = seq(size(seq)) + extrapolate_a(interpolate_a)
end function
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  integer :: n, x, y
  integer, allocatable :: seq(:)
  x = 1
  n = 1
  do
    y = scan(input(1)%p(x:), ' ')
    if (y == 0) exit
    x = x + y
    n = n + 1
  end do
  allocate(seq(n))

  do n =1,size(input)
    p => input(n)%p
    read(p,*) seq
    ans = ans + extrapolate_b(seq)
  end do
end subroutine
recursive function extrapolate_b(seq) result(x)
  implicit none
  integer :: x
  integer :: seq(:)
  integer, allocatable :: interpolate_b(:)
  integer n

  if (all(seq==0)) then
    x = 0
    return
  end if
  allocate(interpolate_b(size(seq)-1))
  do n = 1, size(interpolate_b)
    interpolate_b(n) = seq(n+1) - seq(n)
  end do
  x = seq(1) - extrapolate_b(interpolate_b)
end function
end subroutine
