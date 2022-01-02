subroutine solver24(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer        :: model(14)
  integer(int64) :: ans
  integer        :: part, n, m, nn, mm
  integer        :: a(14),b(14),c(14) 
  type(char_p)   :: input(:)
  integer        :: partner(14),cnt
  integer        :: stack(14),stack_n
  character(len=14) :: ans_c
  integer        :: partsize

  ans = 0
  stack_n = 0
  model = 0
  partsize = size(input)/14
  
  do n = 0,13
    read(input(n*partsize+5)%p(7:),*) a(n+1)
    read(input(n*partsize+6)%p(7:),*) b(n+1)
    read(input(n*partsize+16)%p(7:),*) c(n+1)
  end do
    

  do n = 1,14
    if (a(n) == 26) cycle
    cnt = 1 
    do m = n+1,14
      if (a(m) == 1) cnt = cnt + 1
      if (a(m) == 26) cnt = cnt - 1
      if (cnt == 0) then
        partner(m) = n 
        partner(n) = m
        exit
      end if
    end do
  end do

  if (part == 1) then
    do n = 1,14
      if (a(n) == 1) then
        do nn = 9,1,-1
          do mm = 1,9
            if (c(n) + nn + b(partner(n)) == mm ) then
              model(partner(n)) = mm
              model(n)   = nn
              stack_n = stack_n + 1
              stack(stack_n) = nn + c(n)
              goto 3
            end if
          end do
        end do
3       continue
      else
        stack_n = stack_n - 1
      end if
    end do
  else
    do n = 1,14
      if (a(n) == 1) then
        do nn = 1,9
          do mm = 1,9
            if (c(n) + nn + b(partner(n)) == mm ) then
              model(partner(n)) = mm
              model(n)   = nn
              stack_n = stack_n + 1
              stack(stack_n) = nn + c(n)
              goto 4
            end if
          end do
        end do
4       continue
      else
        stack_n = stack_n - 1
      end if
    end do
  end if


  write(ans_c,'(14(I1))') model
  read(ans_c,*) ans


!  x = mod(z,26)
!  z = z / a
!  x = x + b
!  if (x /= w) then
!    z = z * 26 + (w + c)
!  end if

end subroutine
