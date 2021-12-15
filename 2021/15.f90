subroutine solver15(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, x_l, y_l, x, y, x_o, y_o
  type(char_p)   :: input(:)
  integer*1      :: inval(len(input(1)%p), size(input))
  integer*1      :: temp(len(input(1)%p), size(input))
  integer, allocatable       :: map(:,:)
  integer*1, allocatable     :: val(:,:)
  integer        :: loc(2)
  logical, allocatable :: curr(:, :), done(:, :)
  character(len=20) :: in_l

  x_l = len(input(1)%p)
  y_l = size(input)
  x_o = x_l
  y_o = y_l
  if (part == 2) then
    x_l = x_l * 5
    y_l = y_l * 5
  end if
  allocate(curr(x_l,y_l))
  allocate(done(x_l,y_l))
  allocate(val(x_l,y_l))
  allocate(map(x_l,y_l))

  map = huge(map(1,1))
  map(1,1) = 0
  done = .false.
  curr = .false.
  curr(1,1) = .true.

  write(in_l,*) x_o
  do n=1,y_o
    read(input(n)%p,'('//in_l//'(I1))') inval(:,n)
  end do

  if (part == 1) val = inval
  if (part == 2) then
    do x = 0,4
      do y = 0,4
        temp = mod(inval+int(x+y,1),int(9,1))
        where (temp == 0)
          temp = 9
        end where
        val(x*x_o+1:(x+1)*x_o,y*y_o+1:(y+1)*y_o) =  temp
      end do
    end do
  end if
  
  do while(.not.done(x_l,y_l))
    loc = minloc(map,mask=curr)
    x = loc(1)
    y = loc(2)
    curr(x,y) = .false.
    done(x,y) = .true.

    if (x < x_l) then
      if (.not.done(x+1,y)) then
        map(x+1,y) = min(map(x+1,y),map(x,y) + val(x+1,y))
        curr(x+1,y) = .true.
      end if
    end if
    if (x > 1) then
      if (.not.done(x-1,y)) then
        map(x-1,y) = min(map(x-1,y),map(x,y) + val(x-1,y))
        curr(x-1,y) = .true.
      end if
    end if
    if (y < y_l) then
      if (.not.done(x,y+1)) then
        map(x,y+1) = min(map(x,y+1),map(x,y) + val(x,y+1))
        curr(x,y+1) = .true.
      end if
    end if
    if (y > 1) then
      if (.not.done(x,y-1)) then
        map(x,y-1) = min(map(x,y-1),map(x,y) + val(x,y-1))
        curr(x,y-1) = .true.
      end if
    end if
        
  end do

  ans = map(x_l,y_l)

  deallocate(curr)
  deallocate(done)
  deallocate(val)
  deallocate(map)

end subroutine
