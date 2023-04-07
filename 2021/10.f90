subroutine solver10(part,input,ans)
  use iso_fortran_env, only: int64
  use iso_c_binding
  implicit none
  integer(int64) :: ans
  integer        :: part, n, pos, error, noerror
  integer, parameter :: score(4) = (/3,57,1197,25137/)
  character(len=4),parameter               :: opn = "([{<"
  character(len=4),parameter               :: cls = ")]}>"
  type(char_p) :: input(:)
  integer(int64),target  :: score2(size(input))
  ans = 0
  score2 = 0
  noerror = 0

  do n=1,size(input)
    error = -1
    pos = 1
    do  while ((pos < len(input(n)%p)) .and. (error .eq. -1))
      call dive(input(n)%p,pos,error)
    end do
    if ((error.ne.-1).and.(part == 1)) then
      ans = ans + score(index(cls, input(n)%p(error:error)))
    end if
    if ((error.eq.-1).and.(part == 2)) then
      pos = 1
      call dive2(input(n)%p,pos,score2(n))
      noerror=noerror+1
    end if
  end do
  if (part == 2) then
     call qsort(c_loc(score2),int(size(input),c_size_t) &
                ,c_sizeof(score2(1)),c_funloc(compar))
     ans = score2(size(input)-noerror/2)
  end if
contains

pure recursive subroutine dive(input, pos, error)
  implicit none
  character(len=:), allocatable, intent(inout) :: input
  integer, intent(inout)                   :: pos
  integer, intent(inout)                   :: error
  integer :: nn

  if (pos > len(input)) return

  if (index(cls, input(pos:pos)) .ne. 0) then
    return
  end if

  nn = index(opn, input(pos:pos))
  pos = pos + 1
  do
    call dive(input,pos,error)
    if (pos > len(input)) return
    if (input(pos:pos) == cls(nn:nn)) then
      pos = pos + 1
      return
    else if (index(opn,input(pos:pos)).ne.0) then
      cycle
    else 
      if (error .eq. -1) error = pos
      return
    end if
  end do
end subroutine

recursive subroutine dive2(input, pos, score2)
  implicit none
  character(len=:), allocatable, intent(inout) :: input
  integer, intent(inout)                   :: pos
  integer(int64), intent(inout)            :: score2
  integer :: nn

  if (pos > len(input)) return

  if (index(cls, input(pos:pos)) .ne. 0) then
    return
  end if

  nn = index(opn, input(pos:pos))
  pos = pos + 1
  do
    call dive2(input,pos,score2)
    if (pos > len(input)) then
     score2 = score2*5 + nn
     return
    end if
    if (input(pos:pos) == cls(nn:nn)) then
      pos = pos + 1
      return
    end if
  end do
end subroutine

function compar(a,b)
  integer(int64) :: a,b
  integer(2) :: compar
  if ( a .lt. b ) compar = -1
  if ( a .eq. b ) compar = 0
  if ( a .gt. b ) compar = 1
end function

end subroutine
