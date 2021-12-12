subroutine solver11(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, x_l, y_l, flash, new_flash, x, y, octos
  type(char_p) :: input(:)
  integer        :: map(len(input(1)%p)+2, size(input)+2)
  logical        :: mask(len(input(1)%p)+2, size(input)+2)
  character(len=20) :: in_l

  x_l = len(input(1)%p)+2
  y_l = size(input)+2
  octos = (x_l-2)*(y_l-2)
  flash = 0
  map = 0
  mask = .false.
  ans = 0

  write(in_l,*) x_l -2
  do n=1,size(input)
    read(input(n)%p,'('//in_l//'(I1))') map(2:x_l-1,n+1)
  end do

  if (part == 2) goto 2

  do n = 1,100
    map = map + 1
    new_flash = flash
    flash = flash - 1
    do while (flash /= new_flash)
      flash = new_flash
      do y=2,y_l-1
        do x=2,x_l-1
          if ((map(x,y) > 9) .and. (.not. mask(x,y))) then
            mask(x,y) = .true.
            map(x-1:x+1,y-1:y+1) = map(x-1:x+1,y-1:y+1) + 1
            new_flash = new_flash + 1
          end if
        end do
      end do
    end do
    where (mask)
      map = 0
    end where
    mask = .false.
  end do
  ans = flash
  return

2 n = 0

  do
    n = n + 1
    map = map + 1
    new_flash = 0
    flash = - 1
    do while (flash /= new_flash)
      flash = new_flash
      do y=2,y_l-1
        do x=2,x_l-1
          if ((map(x,y) > 9) .and. (.not. mask(x,y))) then
            mask(x,y) = .true.
            map(x-1:x+1,y-1:y+1) = map(x-1:x+1,y-1:y+1) + 1
            new_flash = new_flash + 1
          end if
        end do
      end do
    end do
    if (new_flash == octos) exit
    where (mask)
      map = 0
    end where
    mask = .false.
  end do
  ans = n
  return
end subroutine
