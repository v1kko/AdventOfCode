subroutine solver12(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n
  type(char_p) :: input(:)

  do n=1,size(input)
    read(input(n)%p,*)
  end do


end subroutine
