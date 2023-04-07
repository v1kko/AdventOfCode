subroutine solver13(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, x, y, n, eq_i, fold_i, max_y, max_x
  logical, save  :: paper(1400,1400)
  type(char_p) :: input(:)
  character(len=2) :: fold
  max_y = 1400
  max_x = 1400
  paper = .false.

  do n=1,size(input)
    if (len(input(n)%p) == 0) exit
    read(input(n)%p,*) x, y
    paper(x+1,y+1) = .true.

  end do

  do n=n+1,size(input)
    eq_i = index(input(n)%p,"=")
    read(input(n)%p(eq_i-1:eq_i-1),*) fold
    read(input(n)%p(eq_i+1:),*) fold_i

    if (fold == "x") then
      max_x = fold_i*2+2
      do x = 1, fold_i
        do y = 1, max_y
          if (paper(max_x-x,y)) then
            paper(x,y) = .true.
          end if
        end do
      end do
      max_x = fold_i
    end if 
    if (fold == "y") then
      max_y = fold_i*2+2
      do x = 1, max_x
        do y = 1, fold_i
          if (paper(x,max_y-y)) then
            paper(x,y) = .true.
          end if
        end do
      end do
      max_y = fold_i
    end if 
    if (part == 1) then
      ans = count(paper(:max_x,:max_y))
      return
    end if
  end do
  ans = 0
  do y = 1, max_y
    do x = 1, max_x
      if (paper(x,y)) then
        write(*,'(A1)', advance="no") '#'
      else
        write(*,'(A1)', advance="no") ' '
      end if
    end do
    write(*,'()')
  end do
end subroutine
