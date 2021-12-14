subroutine solver14(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, maxlevel,x,y,idx
  integer(int64) :: maxv, minv
  integer, parameter :: ca = ICHAR('A') - 1
  type(char_p) :: input(:)
  character(len=2) :: pairs(size(input)-2)
  character(len=1) :: insert(size(input)-2)
  character(len=4) :: dum
  integer(int64)   :: cnt(26)
  integer(int64)   :: cnt_p(26,26,26)
  integer(int64)   :: cnt_p_new(26,26,26)
  integer          :: newchar

  cnt_p = 0
  cnt = 0
  do n=3,size(input)
    read(input(n)%p,'(A2A4A1)') pairs(n-2), dum, insert(n-2)
  end do

  if (part == 1) maxlevel = 10
  if (part == 2) maxlevel = 40

   
  do n = 1, maxlevel
    do x = 1,26
      do y = 1,26
        idx = findloc(pairs,char(x+ca)//char(y+ca),1) 
        if (idx /= 0) then
          newchar = ichar(insert(idx))-ca
          cnt_p_new(:,x,y) = cnt_p(:,x,newchar) &
                           + cnt_p(:,newchar,y) 
          cnt_p_new(newchar,x,y) = cnt_p_new(newchar,x,y) + 1
        else
          cnt_p_new(:,x,y) = cnt_p(:,x,y)
        end if
      end do
    end do
    cnt_p = cnt_p_new
  end do

  do n = 1, len(input(1)%p)-1
    cnt = cnt + cnt_p(:,ichar(input(1)%p(n:n))-ca,ichar(input(1)%p(n+1:n+1))-ca)
  end do
  do n = 1, len(input(1)%p)
    cnt(ichar(input(1)%p(n:n))-ca) = cnt(ichar(input(1)%p(n:n))-ca) + 1
  end do

  maxv = maxval(cnt)
  minv = maxv
  do n = 1, 26
    if (cnt(n) == 0) cycle
    if (minv > cnt(n)) minv = cnt(n)
  end do

  ans = maxv - minv
  return

end subroutine
