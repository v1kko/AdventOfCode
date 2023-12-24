subroutine solver11(part,input,ans)
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
  character(len=:), pointer :: p
  logical, allocatable :: map(:,:)
  integer :: n, m, minx, miny, maxx, maxy
  integer, allocatable :: emptyx(:), emptyy(:), stars(:,:)
  integer :: nex, ney, nstars
  ans = 0
  nex = 0
  ney = 0
  nstars = 0

  allocate(map(len(input(1)%p),size(input)))
  allocate(emptyx(size(map,1)), emptyy(size(map,2)))
  allocate(stars(2,size(map)))
  map = .false.

  do n = 1,size(input)
    p => input(n)%p
    do m = 1, len(p)
      if (p(m:m) == '#') then
        map(m,n) = .true.
        nstars = nstars + 1
        stars(:,nstars) = (/m,n/)
      end if
    end do
  end do
  do n = 1,size(map,1)
    if (.not. any(map(n,:))) then
      nex = nex + 1
      emptyx(nex) = n
    end if
  end do
  do n = 1,size(map,2)
    if (.not. any(map(:,n))) then
      ney = ney + 1
      emptyy(ney) = n
    end if
  end do
  do n = 1, nstars-1
    do m = n + 1, nstars
      minx = min(stars(1,n),stars(1,m))
      maxx = max(stars(1,n),stars(1,m))
      miny = min(stars(2,n),stars(2,m))
      maxy = max(stars(2,n),stars(2,m))
      
      ans = ans + count(emptyx(:nex) > minx .and. emptyx(:nex) < maxx)
      ans = ans + count(emptyy(:ney) > miny .and. emptyy(:ney) < maxy)
      ans = ans + (maxy - miny) + (maxx - minx)
    end do
  end do

end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  logical, allocatable :: map(:,:)
  integer :: n, m, minx, miny, maxx, maxy
  integer, allocatable :: emptyx(:), emptyy(:), stars(:,:)
  integer :: nex, ney, nstars
  ans = 0
  nex = 0
  ney = 0
  nstars = 0

  allocate(map(len(input(1)%p),size(input)))
  allocate(emptyx(size(map,1)), emptyy(size(map,2)))
  allocate(stars(2,size(map)))
  map = .false.

  do n = 1,size(input)
    p => input(n)%p
    do m = 1, len(p)
      if (p(m:m) == '#') then
        map(m,n) = .true.
        nstars = nstars + 1
        stars(:,nstars) = (/m,n/)
      end if
    end do
  end do
  do n = 1,size(map,1)
    if (.not. any(map(n,:))) then
      nex = nex + 1
      emptyx(nex) = n
    end if
  end do
  do n = 1,size(map,2)
    if (.not. any(map(:,n))) then
      ney = ney + 1
      emptyy(ney) = n
    end if
  end do
  do n = 1, nstars-1
    do m = n + 1, nstars
      minx = min(stars(1,n),stars(1,m))
      maxx = max(stars(1,n),stars(1,m))
      miny = min(stars(2,n),stars(2,m))
      maxy = max(stars(2,n),stars(2,m))
      
      ans = ans + 999999 * count(emptyx(:nex) > minx .and. emptyx(:nex) < maxx)
      ans = ans + 999999 * count(emptyy(:ney) > miny .and. emptyy(:ney) < maxy)
      ans = ans + (maxy - miny) + (maxx - minx)
    end do
  end do

end subroutine
end subroutine
