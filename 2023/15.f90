subroutine solver15(part,input,ans)
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
  character(len=:), pointer :: p, q
  integer :: n
  ans = 0
  p => input(1)%p
  do 
    n = scan(p,',')
    if (n == 0) then
      n = len(p)
    else
      n = n - 1
    end if
    q => p(:n)
    ans = ans + hash(q)
    if (n == len(p)) exit
    p => p(n+2:)
  end do
end subroutine
function hash(p) result(val)
  implicit none
  character(len=:), pointer :: p
  integer :: val, n
  val = 0
  do n = 1, len(p)
    val = val + iachar(p(n:n))     
    val = val * 17
    val = mod(val,256)
  end do

end function
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p, q
  type entry
    integer :: f
    character(len=:), pointer :: p
  end type
  type(entry) :: boxes(10,0:255)
  integer :: boxn(0:255) = 0
  integer :: m, n, h, f, l


  ans = 0
  p => input(1)%p
  do 
    n = scan(p,',')
    if (n == 0) then
      n = len(p)
    else
      n = n - 1
    end if
    q => p(:n)
    m = scan(p,'-=')
    q => q(:m-1)
    h = hash(q)
    select case(p(m:m))
    case('=')
      read(p(m+1:n),*) f
      do l=1,boxn(h)
        if (boxes(l,h)%p == q) then
          boxes(l,h)%f = f
          goto 2
        end if
      end do
      boxn(h) = boxn(h) + 1
      boxes(boxn(h),h)%f = f
      boxes(boxn(h),h)%p => q 
2     continue
    case('-')
      do l=1,boxn(h)
        if (boxes(l,h)%p == q) then
          boxes(l:boxn(h)-1,h) = boxes(l+1:boxn(h),h)
          boxn(h) = boxn(h) -1
          exit
        end if
      end do
    end select
    if (n == len(p)) exit
    p => p(n+2:)
  end do
  do n = 0,255
    do m = 1, boxn(n)
      ans = ans + (n+1)*m*boxes(m,n)%f
    end do
  end do
end subroutine
end subroutine
