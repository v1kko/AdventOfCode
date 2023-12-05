subroutine solver4(part,input,ans)
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
  integer :: n, m, s, d, g, matches
  integer :: nw, w(10)

  do n =1,size(input)
    p => input(n)%p
    s = scan(p, ':') + 2
    nw = 1
    do
      d = s+1
      read(p(s:d),'(i2)') w(nw)
      s = d + 2
      if (p(s:s) == '|') exit
      nw = nw + 1
    end do
    s = s + 2
    matches = 0
    do
      d = s+1
      read(p(s:d),'(i2)') g
      do m = 1, 10
        if (w(m) == g) then
          matches = matches + 1
          exit
        end if
      end do
      s = d+2
      if (s > len(p)) exit
    end do
    if (matches /= 0) ans = ans + 2**(matches-1)
  end do
end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  integer :: n, m, s, d, g, matches
  integer :: nw, w(10)
  integer :: ns(size(input))
  ns = 1

  do n =1,size(input)
    p => input(n)%p
    s = scan(p, ':') + 2
    nw = 1
    do
      d = s+1
      read(p(s:d),'(i2)') w(nw)
      s = d + 2
      if (p(s:s) == '|') exit
      nw = nw + 1
    end do
    s = s + 2
    matches = 0
    do
      d = s+1
      read(p(s:d),'(i2)') g
      do m = 1, 10
        if (w(m) == g) then
          matches = matches + 1
          exit
        end if
      end do
      s = d+2
      if (s > len(p)) exit
    end do
    do m = n+1,n+matches
      if (m > size(ns)) exit
      ns(m) = ns(m) + ns(n)
    end do
  end do
  ans = sum(ns)
end subroutine
end subroutine
