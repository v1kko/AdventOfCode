subroutine solver25(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  type(char_p)   :: input(:)
  integer        :: part, n, m
  integer        :: in_l, in_s
  integer*1      :: seabed(len(input(1)%p),size(input))
  integer*1      :: seabed2(len(input(1)%p),size(input))
  integer*1      :: seabed3(len(input(1)%p),size(input))
  character(len=1)::buf

  in_l = len(input(1)%p)
  in_s = size(input)

  do n=1,in_s
    do m=1,in_l
      read(input(n)%p(m:m),'(A1)') buf
      if (buf == ".") seabed3(m,n) = 0
      if (buf == ">") seabed3(m,n) = 1
      if (buf == "v") seabed3(m,n) = 2
    end do
  end do

  ans = 0
  do 
    ans = ans + 1
    seabed = seabed3
    seabed2 = 0
    do n=1,size(input)
      do m=1,len(input(n)%p)
        if (seabed(m,n) == 2) seabed2(m,n) = 2
        if (seabed(m,n) == 1) then
          if (seabed(mod(m,in_l)+1,n) == 0) then
            seabed2(mod(m,in_l)+1,n) = 1
          else
            seabed2(m,n) = 1
          end if
        end if 
      end do
    end do

    seabed3 = 0
    do n=1,size(input)
      do m=1,len(input(n)%p)
        if (seabed2(m,n) == 1) seabed3(m,n) = 1
        if (seabed2(m,n) == 2) then
          if (seabed2(m,mod(n,in_s)+1) == 0) then
            seabed3(m,mod(n,in_s)+1) = 2
          else
            seabed3(m,n) = 2
          end if
        end if 
      end do
    end do
    if (all((seabed3-seabed)==0)) exit  
  end do
  if (part == 2) ans = 0
end subroutine
