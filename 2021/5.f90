subroutine solver5(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer :: part,n, x1,x2,y1,y2,x,y,it_x,it_y,i
  type(char_p) :: input(:)
  character(len=10) :: dum
  integer, save :: map(1000,1000)
  logical, save :: cross(1000,1000)

  map = 0
  cross = .false.

  do n=1,size(input)
    read(input(n)%p,*)x1,y1,dum,x2,y2
    if ((part==1) .and. (x1 /= x2) .and. (y1 /= y2)) cycle

    it_x = 1
    it_y = 1
    if (x1 > x2) it_x = -1
    if (y1 > y2) it_y = -1

    if ((x1 == x2) .or. (y1 == y2)) then
      do x = x1,x2,it_x
        do y = y1,y2,it_y
          map(x,y) = map(x,y) + 1
        end do
      end do
    else
      do i = 0,abs(x1-x2)
          map(x1+i*it_x,y1+i*it_y) = map(x1+i*it_x,y1+i*it_y) + 1
      end do
    end if
  end do
  
  where ( map > 1 )
    cross = .true.
  end where
  ans = count(cross)
end subroutine
