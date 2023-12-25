subroutine solver13(part,input,ans)
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
  integer :: n, m
  ans = 0

  m = 1
  do n =1,size(input)
    if (input(n)%p == "") then
      ans = ans + process_a(input(m:n-1))
      m = n+1
    end if
  end do
  ans = ans + process_a(input(m:))
end subroutine
function process_a(input) result(ans)
  implicit none
  integer :: ans, n, m, s, d
  type(char_p), target :: input(:)
  logical, allocatable :: picture(:,:)
  allocate(picture(len(input(1)%p),size(input)))
  picture = .false.
  ans = 0
  do n=1,size(input)
    do m=1,len(input(n)%p)
      picture(m,n) = input(n)%p(m:m) == '#'
    end do
  end do
  do d = 1,2
 c: do s=1,size(picture,d)-1
      m = max(1,s-(size(picture,d)-s)+1)
      do n = m,s
        m = (s+s+1)-n
        if (d == 1) then
          if (.not. all(picture(n,:) .eqv. picture(m,:))) cycle c 
        else
          if (.not. all(picture(:,n) .eqv. picture(:,m))) cycle c 
        end if
      end do
      ans = 99 * (d-1) * s + s
      return
    end do c
  end do
end function
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  integer :: n, m 
  ans = 0

  m = 1
  do n =1,size(input)
    if (input(n)%p == "") then
      ans = ans + process_b(input(m:n-1))
      m = n+1
    end if
  end do
  ans = ans + process_b(input(m:))
end subroutine
function process_b(input) result(ans)
  implicit none
  integer :: ans, n, m, s, d, cnt
  type(char_p), target :: input(:)
  logical, allocatable :: picture(:,:)
  allocate(picture(len(input(1)%p),size(input)))
  picture = .false.
  ans = 0
  do n=1,size(input)
    do m=1,len(input(n)%p)
      picture(m,n) = input(n)%p(m:m) == '#'
    end do
  end do
  do d = 1,2
 c: do s=1,size(picture,d)-1
      cnt = 0
      m = max(1,s-(size(picture,d)-s)+1)
      do n = m,s
        m = (s+s+1)-n
        if (d == 1) then
          cnt = cnt + count(picture(n,:) .neqv. picture(m,:)) 
        else
          cnt = cnt + count(picture(:,n) .neqv. picture(:,m)) 
        end if
        if (cnt > 1) cycle c
      end do
      if (cnt /= 1) cycle c
      ans = 99 * (d-1) * s + s
      return
    end do c
  end do
end function
end subroutine
