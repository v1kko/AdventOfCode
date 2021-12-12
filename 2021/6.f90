subroutine solver6(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part
  integer(int64) :: n,nn, draw, next_c, prev_c, temp
  integer(int64) :: fish(0:8)
  type(char_p)   :: input(:)

  fish = 0
  prev_c = 1
  do
    next_c = prev_c + index(input(1)%p(prev_c+1:),",")
    if (next_c .eq. prev_c) then
      read(input(1)%p(prev_c:),*) draw
      fish(draw) = fish(draw) + 1
      exit
    else
      read(input(1)%p(prev_c:next_c),*) draw
    endif
    prev_c = next_c + 1
    fish(draw) = fish(draw) + 1
  end do

if (part==2) goto 2
  do n = 1,80
    temp = fish(0)
    do nn = 0,7
      fish(nn) = fish(nn+1)
    end do
    fish(8) = temp
    fish(6) = fish(6) + temp
  end do
  ans = sum(fish)
  return

2 do n = 1,256
    temp = fish(0)
    do nn = 0,7
      fish(nn) = fish(nn+1)
    end do
    fish(8) = temp
    fish(6) = fish(6) + temp
  end do
  ans = sum(fish)
  return

end subroutine
