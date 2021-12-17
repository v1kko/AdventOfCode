subroutine solver17(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, x(2), y(2), idx(2), xx, yy, x3, y3
  integer        :: loc(2)
  integer        :: max_x, min_x, min_y, max_y
  type(char_p)   :: input(:)

  idx(1) =  index(input(1)%p,"=")
  idx(2) =  index(input(1)%p,".")
  read(input(1)%p(idx(1)+1:idx(2)-1),*) x(1)
  idx(1) =  idx(2)+1
  idx(2) =  index(input(1)%p(idx(1):),"=") + idx(1) -1
  read(input(1)%p(idx(1)+1:idx(2)-4),*) x(2)
  idx(1) = idx(2)
  idx(2) =  index(input(1)%p(idx(1):),".") + idx(1) -1
  read(input(1)%p(idx(1)+1:idx(2)-1),*) y(1)
  idx(1) = idx(2) + 1
  read(input(1)%p(idx(1)+1:),*) y(2)


  max_x = x(2)
  min_x = sign(int((1+8*abs(x(1)))**0.5/2),x(1))
  max_y = max(abs(y(1)),abs(y(2)))
  min_y = min(y(1),y(2))
  if (part == 1) then
    ans = max_y*(max_y-1)/2
    return
  end if

  ans = 0
  do yy = min_y, max_y
    do xx = min_x, max_x
      x3 = xx
      y3 = yy
      loc = 0
      do while(loc(2) > min(y(1),y(2)))
        loc = loc + (/x3,y3/)
        if (    (loc(1) >= x(1)) &
          .and. (loc(1) <= x(2)) &
          .and. (loc(2) >= y(1)) &
          .and. (loc(2) <= y(2))) then
          ans = ans + 1
          exit
        end if
        if (x3 /= 0) x3 = x3 - sign(1,x3)
        y3 = y3 - 1
      end do
    end do
  end do

end subroutine
