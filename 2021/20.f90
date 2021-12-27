subroutine solver20(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  type(char_p)   :: input(:)
  integer        :: part, n, m, idx,r,x,passes
  integer, allocatable :: picture(:,:), newpicture(:,:)
  integer        :: dict(len(input(1)%p))
  character(len=1)::buf

  if (part == 1) passes = 2
  if (part == 2) passes = 50


  allocate(picture(len(input(3)%p)+4*passes+2,size(input)+4*passes))
  allocate(newpicture(len(input(3)%p)+4*passes+2,size(input)+4*passes))

  do m =1, len(input(1)%p)
    read(input(1)%p(m:m),'(A1)') buf
    if (buf == "#") dict(m) = 1
    if (buf == ".") dict(m) = 0
  end do

  picture = 0

  do n=3,size(input)
    do m =1, len(input(3)%p)
      read(input(n)%p(m:m),'(A1)') buf
      if (buf == "#") picture(m+passes*2+1,n-1+passes*2) = 1
      if (buf == ".") picture(m+passes*2+1,n-1+passes*2) = 0
    end do
  end do

  do r =1,passes
    newpicture = 0
    do n = 2, size(picture,2)-1
      do m = 2, size(picture,1)-1
        idx = 0
        do x = 0,2
          idx = idx + picture(m + 1 - mod(x,3),n + 1)*2**x
        end do
        do x = 3,5
          idx = idx + picture(m + 1 - mod(x,3),n)*2**x
        end do
        do x = 6,8
          idx = idx + picture(m + 1 - mod(x,3),n - 1)*2**x
        end do
        newpicture(m,n) = dict(idx+1) 
      end do
    end do
    picture = newpicture
  end do

  picture(1:passes+1,:) = 0
  picture(size(picture,1)-passes:size(picture,1),:) = 0
  picture(:,1:passes+1) = 0
  picture(:,size(picture,2)-passes:size(picture,2)) = 0
  
  ans  = sum(picture)

end subroutine
