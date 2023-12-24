subroutine solver10(part,input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  integer :: part
  ans = 0
  select case(part)
  case(1)
    call solver_a(input,ans)
  case(2)
    call solver_b(input,ans)
  end select
contains
subroutine solver_a(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  integer :: n, m, fdir = 0, bdir = 0
  integer :: s(2) = 0, f(2) = 0, b(2) = 0, dirs(2,4)
  character, allocatable :: map(:,:)

  allocate(map(len(input(1)%p),size(input)))
  do n =1,size(map,2)
    do m = 1, size(map,1)
      map(m,n) = input(n)%p(m:m)
      if ( map(m,n) == 'S' ) s = (/m,n/)
    end do
  end do
  dirs = reshape((/1,0, 0, 1, -1, 0, 0, -1/),(/2,4/))
  do n = 1, 4
    if (any(s + dirs(:,n) < 1) .or. s(1) + dirs(1,n) > size(map,1) .or. &
       s(2) + dirs(2,n) > size(map,2) ) cycle
    fdir = next(n,map(s(1) + dirs(1,n),s(2) + dirs(2,n)))
    if (fdir /= -1) then
      f = s + dirs(:,n)
      exit
    end if
  end do
  do n = n+1, 4
    bdir = next(n,map(s(1) + dirs(1,n),s(2) + dirs(2,n)))
    if (bdir /= -1) then
      b = s + dirs(:,n)
      exit
    end if
  end do
  n = 1
  do
    n = n + 1
    f = f + dirs(:,fdir)
    fdir = next(fdir,map(f(1),f(2)))
    b = b + dirs(:,bdir)
    bdir = next(bdir,map(b(1),b(2)))
    if (all(f.eq.b)) then
      ans = n
      exit
    end if
  end do
end subroutine
function next(dir, tile)
  implicit none
  integer dir, next
  character :: tile
  next = -1
  select case(tile)
  case('7') 
    if (dir == 1) next = 2
    if (dir == 4) next = 3
  case('L') 
    if (dir == 3) next = 4
    if (dir == 2) next = 1
  case('J') 
    if (dir == 1) next = 4
    if (dir == 2) next = 3
  case('F') 
    if (dir == 4) next = 1
    if (dir == 3) next = 2
  case('-') 
    if (dir == 1) next = 1
    if (dir == 3) next = 3
  case('|') 
    if (dir == 2) next = 2
    if (dir == 4) next = 4
  end select
end function
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  integer :: n, m, fdir = 0
  integer :: s(2) = 0, f(2) = 0, dirs(2,4)
  character, allocatable :: map(:,:)
  logical, allocatable :: exploded(:,:)
  integer, allocatable :: fill(:,:)
  integer :: nfill

  allocate(map(len(input(1)%p),size(input)))
  allocate(exploded(len(input(1)%p)*3,size(input)*3))
  allocate(fill(2,size(exploded)))
  exploded = .false.

  do n =1,size(map,2)
    do m = 1, size(map,1)
      map(m,n) = input(n)%p(m:m)
      if ( map(m,n) == 'S' ) s = (/m,n/)
    end do
  end do
  dirs = reshape((/1,0, 0, 1, -1, 0, 0, -1/),(/2,4/))
  do n = 1, 4
    if (any(s + dirs(:,n) < 1) .or. s(1) + dirs(1,n) > size(map,1) .or. &
       s(2) + dirs(2,n) > size(map,2) ) cycle
    fdir = next(n,map(s(1) + dirs(1,n),s(2) + dirs(2,n)))
    if (fdir /= -1) then
      f = s + dirs(:,n)
      exit
    end if
  end do
  call paint(map, exploded, f)
  do
    f = f + dirs(:,fdir)
    call paint(map, exploded, f)
    fdir = next(fdir,map(f(1),f(2)))
    if (all(f == s)) then
      exit
    end if
  end do
  nfill = 1
  fill(:,nfill) = (/1,1/)
  exploded(1,1) = .true.
  do while (nfill > 0)
    do n = 1, 4
      if (any(fill(:,1) + dirs(:,n) < 1) .or. fill(1,1) + dirs(1,n) > size(exploded,1) .or. &
       fill(2,1) + dirs(2,n) > size(exploded,2) ) cycle
      if (.not. exploded(fill(1,1)+dirs(1,n),fill(2,1)+ dirs(2,n))) then
        exploded(fill(1,1)+dirs(1,n),fill(2,1)+ dirs(2,n)) = .true.
        nfill = nfill + 1 
        fill(:,nfill) = fill(:,1) + dirs(:,n)
      end if
    end do
    fill(:,1) = fill(:,nfill)
    nfill = nfill - 1
  end do
  do n = 0, size(map,2)-1
    do m = 0, size(map,1)-1
      if (.not. any( exploded(m*3+1:m*3+3,n*3+1:n*3+3))) ans = ans + 1 
    end do
  end do

end subroutine
subroutine paint(map, exploded, pos)
  implicit none
  integer :: pos(2)
  character :: map(:,:)
  character :: tile
  logical, target :: exploded(:,:)
  logical, pointer :: canvas(:,:)
  canvas => exploded((pos(1)-1)*3+1:(pos(1)-1)*3+3,(pos(2)-1)*3+1:(pos(2)-1)*3+3)
  tile = map(pos(1),pos(2))
  select case(tile)
    case('J')
      canvas = reshape((/.false.,.true.,.false.,.true.,.true.,.false.,.false.,.false.,.false./),(/3,3/))
    case('F')
      canvas = reshape((/.false.,.false.,.false.,.false.,.true.,.true.,.false.,.true.,.false./),(/3,3/))
    case('7')
      canvas = reshape((/.false.,.false.,.false.,.true.,.true.,.false.,.false.,.true.,.false./),(/3,3/))
    case('L')
      canvas = reshape((/.false.,.true.,.false.,.false.,.true.,.true.,.false.,.false.,.false./),(/3,3/))
    case('-')
      canvas = reshape((/.false.,.false.,.false.,.true.,.true.,.true.,.false.,.false.,.false./),(/3,3/))
    case('|')
      canvas = reshape((/.false.,.true.,.false.,.false.,.true.,.false.,.false.,.true.,.false./),(/3,3/))
    case('S')
      canvas = reshape((/.false.,.true.,.false.,.true.,.true.,.true.,.false.,.true.,.false./),(/3,3/))
  end select
end subroutine
end subroutine
