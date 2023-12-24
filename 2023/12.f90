subroutine solver12(part,input,ans)
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
  character(len=:), pointer :: p, r
  integer :: n, s, d
  integer :: nums(100), nn
  ans = 0

  do n =1,size(input)
    p => input(n)%p
    nn = 0
    s = scan(p,' ') + 1
    r => p(:s-2)
    do
      d = scan(p(s:),',')
      if (d == 0) then
        d = len(p)
      else 
        d = d + s -2
      end if
      nn = nn + 1
      read(p(s:d),*) nums(nn)
      if (d == len(p)) exit
      s = d + 2
    end do
    s = scan(r,'#?')
    if (s == 0) cycle
    r => r(s:)
    ans = ans + solve_a(r,nums(:nn))

  end do
end subroutine
recursive function solve_a(p, nums) result(possible)
  implicit none
  character(len=:), pointer :: p,r,q
  integer :: nums(:)
  integer :: possible, cur, x
  possible = 0
  r => p
  cur = nums(1)
  do
    if (sum(nums)+(size(nums)-1) > len(r)) exit

    x = scan(r(:cur),'.')
    if (x /= 0) goto 2

    if (cur < len(r)) then
      if ( r(cur+1:cur+1) == '#') goto 2
    end if

    if (size(nums) == 1) then
      if (scan(r(cur+2:),'#') == 0) possible = possible + 1
    else
      x = scan(r(cur+2:),'?#')
      if ( x == 0 ) exit
      q => r(cur+1+x:)
      possible = possible + solve_a(q,nums(2:))
    end if
2   continue
    if (r(1:1) == '#') exit
    x = scan(r(2:),'#?')
    if (x==0) exit
    r => r(x+1:)
  end do
end function
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p, r
  character(len=:), allocatable, target :: x
  integer :: n, s, d
  integer :: nums(100), nn, ncache(100)
  integer(int64), allocatable :: cache(:,:)
  ans = 0

  allocate(cache(110,100))

  do n =1,size(input)
    p => input(n)%p
    nn = 0
    s = scan(p,' ') + 1
    x = p(:s-2)
    do
      d = scan(p(s:),',')
      if (d == 0) then
        d = len(p)
      else 
        d = d + s -2
      end if
      nn = nn + 1
      read(p(s:d),*) nums(nn)
      if (d == len(p)) exit
      s = d + 2
    end do
    do s = 1,5
      nums(s*nn+1:(s+1)*nn)=nums(:nn)
    end do
    nn = nn*5 
    x = x // '?' // x // '?' // x // '?' // x // '?' // x
    r => x
    s = scan(r,'#?')
    if (s == 0) cycle
    r => r(s:)

    ncache = 0
    cache = -1
    ans = ans + solve_b(r,nums(:nn),cache(:,:nn))
  end do
end subroutine
recursive function solve_b(p, nums, cache) result(possible)
  implicit none
  character(len=:), pointer :: p,r,q
  integer :: nums(:)
  integer(int64) :: possible, cache(:,:)
  integer :: cur, x
  r => p
  cur = nums(1)
  possible = cache(len(p),1)
  if (possible > -1) then
      return
  else
    possible = 0
  end if
  do
    if (sum(nums)+(size(nums)-1) > len(r)) exit

    x = scan(r(:cur),'.')
    if (x /= 0) goto 2

    if (cur < len(r)) then
      if ( r(cur+1:cur+1) == '#') goto 2
    end if

    if (size(nums) == 1) then
      if (scan(r(cur+2:),'#') == 0) possible = possible + 1
    else
      x = scan(r(cur+2:),'?#')
      if ( x == 0 ) exit
      q => r(cur+1+x:)
      possible = possible + solve_b(q,nums(2:),cache(:,2:))
    end if
2   continue
    if (r(1:1) == '#') exit
    x = scan(r(2:),'#?')
    if (x==0) exit
    r => r(x+1:)
  end do
  cache(len(p),1) = possible
end function
end subroutine
