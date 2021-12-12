subroutine solver9(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, x_l, y_l, x, y, n_point
  type(char_p)   :: input(:)
  integer        :: map(len(input(1)%p)+2, size(input)+2)
  integer        :: mask(len(input(1)%p)+2, size(input)+2)
  integer, allocatable:: bassins(:)
  integer        :: loc
  character(len=20) :: in_l

  x_l = len(input(1)%p)+2
  y_l = size(input)+2
  map = 9
  mask = 0
  ans = 0
  n_point = 0

  write(in_l,*) x_l -2
  do n=1,size(input)
    read(input(n)%p,'('//in_l//'(I1))') map(2:x_l-1,n+1)
  end do

  do x = 2,x_l-1
    do y = 2,y_l-1
      if (   (map(x-1,y) > map(x,y)) &
        .and.(map(x+1,y) > map(x,y)) &
        .and.(map(x,y+1) > map(x,y)) &
        .and.(map(x,y-1) > map(x,y))) then
        if (part == 1) then 
          ans = ans + map(x,y) + 1
        else
          n_point = n_point + 1
          mask(x,y) = n_point
        end if
      end if
    end do
  end do
  if (part == 1) return

  allocate(bassins(n_point),source=1)
  do n = 1,9
    do x = 2,x_l-1
      do y = 2,y_l-1
        if (map(x,y) == 9) cycle 
        if (mask(x,y) .ne. 0) cycle
        if ((mask(x-1,y) .ne. 0) .and. (map(x-1,y) < map(x,y))) then
          mask(x,y) = mask(x-1,y)
        else if ((mask(x+1,y) .ne. 0) .and. (map(x+1,y) < map(x,y))) then
          mask(x,y) = mask(x+1,y)
        else if ((mask(x,y-1) .ne. 0) .and. (map(x,y-1) < map(x,y))) then
          mask(x,y) = mask(x,y-1)
        else if ((mask(x,y+1) .ne. 0) .and. (map(x,y+1) < map(x,y))) then
          mask(x,y) = mask(x,y+1)
        end if
         if (mask(x,y) .ne. 0) then
          bassins(mask(x,y)) = bassins(mask(x,y)) + 1
        end if
      end do
    end do
  end do
  ans = 1
  do n=1,3
    loc = maxloc(bassins,1)
    ans = ans * bassins(loc)
    bassins(loc) = 0
  end do

  deallocate(bassins)

          



end subroutine
