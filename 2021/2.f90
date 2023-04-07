subroutine solver2(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer :: part, dist, fw, ud, n, aim
  type(char_p) :: input(:)
  character(len=20) :: direction

  ans = 0
  fw = 0
  ud = 0

  if (part == 2) goto 2

  do n =1,size(input)
    read(input(n)%p,*) direction, dist
    if (direction == "up") then
      ud = ud - dist
    else if (direction == "down") then
      ud = ud + dist
    else if (direction == "forward") then
      fw = fw + dist
    else
      stop
    end if
  enddo
  ans = fw * ud
  return

2 aim = 0

  do n =1,size(input)
    read(input(n)%p,*) direction, dist
    if (direction == "up") then
      aim = aim - dist
    else if (direction == "down") then
      aim = aim + dist
    else if (direction == "forward") then
      fw = fw + dist
      ud = ud + dist * aim
    else
      stop
    end if
  enddo
  ans = fw * ud

  return
  
end subroutine
