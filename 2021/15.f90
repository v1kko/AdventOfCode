subroutine solver15(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, x_l, y_l, x, y, x_o, y_o, idx
  type(char_p)   :: input(:)
  integer*1      :: inval(len(input(1)%p), size(input))
  integer*1      :: temp(len(input(1)%p), size(input))
  integer        :: pque_c
  character(len=20) :: in_l
  integer  , allocatable     :: map(:,:)
  integer  , allocatable     :: pque(:), pque_v(:,:)
  integer*1, allocatable     :: val(:,:)
  logical, allocatable       :: curr(:, :), done(:, :)

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
  allocate(pque(x_l*y_l))
  allocate(pque_v(2,x_l*y_l))

  map = huge(map(1,1))
  map(1,1) = 0
  done = .false.
  curr = .false.
  curr(1,1) = .true.
  pque_c = 1
  pque_v(1:2,1) = (/1,1/)
  pque(1) = 1

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
    idx = minloc(pque(1:pque_c),1)
    x = pque_v(1,idx)
    y = pque_v(2,idx)
    curr(x,y) = .false.
    done(x,y) = .true.

    if (x < x_l) then
      call work(x+1,y,x,y)
    end if
    if (x > 1) then
      call work(x-1,y,x,y)
    end if
    if (y < y_l) then
      call work(x,y+1,x,y)
    end if
    if (y > 1) then
      call work(x,y-1,x,y)
    end if

    pque(idx) = pque(pque_c)
    pque_v(1:2,idx) = pque_v(1:2,pque_c)
    pque_c = pque_c -1
        
  end do

  ans = map(x_l,y_l)

  deallocate(curr)
  deallocate(done)
  deallocate(val)
  deallocate(map)
  deallocate(pque)
  deallocate(pque_v)

contains 

subroutine work(xx, yy, x, y)
  implicit none
  integer, intent(in) :: xx, yy, x, y
  integer             :: idx

  if (.not.done(xx,yy)) then
    map(xx,yy) = min(map(xx,yy),map(x,y) + val(xx,yy))
    if (curr(xx,yy)) then
      do idx = 1,pque_c
        if (all(pque_v(1:2,idx).eq.(/xx,yy/))) exit
      end do
      pque(idx) =  map(xx,yy)
    else 
      pque_c = pque_c + 1
      pque(pque_c) = map(xx,yy)
      pque_v(1:2,pque_c) = (/ xx,yy /)
      curr(xx,yy) = .true.
    end if
  end if
end subroutine


end subroutine
