subroutine solver1(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer :: part, n, prev, cur, buf, nn
  type(char_p) :: input(:)

  ans = 0

  if (part == 2) goto 2

  read(input(1)%p,*) cur
  do n =2,size(input)
    prev = cur 
    read(input(n)%p,*) cur
    if (cur > prev) ans = ans + 1
  enddo
  return

2 cur = 0
  do n = 1,3
    read(input(n)%p,*) buf
    cur = cur + buf
  end do
  do n =2,size(input)-2
    prev = cur 
    cur = 0
    do nn = n,n+2
      read(input(nn)%p,*) buf
      cur = cur + buf
    end do
    if (cur > prev) ans = ans + 1
  enddo
  return
  
end subroutine
