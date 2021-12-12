subroutine solver12(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64)   :: ans
  integer          :: part, n, sep, x_in, y_in, map_size
  type(char_p)     :: input(:)
  character(len=6) :: map(size(input)), x, y
  logical :: links(size(input),size(input))
  logical :: capital(size(input))
  logical :: visited(size(input))
  integer          :: root, last
  integer          :: paths

  map = ""
  map_size = 0
  links = .false.
  ans = 0
  capital = .false.
  visited = .false.

  do n=1,size(input)
    sep = index(input(n)%p, "-")
    read(input(n)%p(:sep-1),*) x
    read(input(n)%p(sep+1:),*) y
    if (findloc(map,x,1) == 0) then
      map_size = map_size + 1
      map(map_size) = x 
    end if
    if (findloc(map,y,1) == 0) then
      map_size = map_size + 1
      map(map_size) = y 
    end if

    x_in = findloc(map,x,1)
    y_in = findloc(map,y,1)
    
    links(x_in, y_in) = .true.
    links(y_in, x_in) = .true.

  end do

  root = findloc(map, "start", 1)
  last = findloc(map, "end", 1)

  do n = 1,map_size
    if (LLT(map(n)(1:1), 'a')) then 
      capital(n) = .true.
    end if
  end do

  paths = 0
  if (part == 1) then
    call walk(root,paths,visited)
  else
    call walk2(root,paths,visited,.false.)
  end if
  ans = paths

contains

pure recursive subroutine walk (cur,paths, visited)
  integer,intent(inout) :: paths
  integer,intent(in) :: cur
  logical,intent(in) :: visited(size(input))
  integer :: n
  logical :: visited2(size(input))
  if (cur == last) then
    paths = paths + 1
    return
  end if
  if (visited(cur)) return
  visited2 = visited
  if (.not.capital(cur)) visited2(cur) = .true.
  do n = 1,map_size
    if (links(cur,n)) then
      call walk(n,paths,visited2)
    end if
  end do

end subroutine

recursive subroutine walk2 (cur,paths, visited, doubled)
  integer,intent(inout) :: paths
  integer,intent(in) :: cur
  logical,intent(in) :: visited(size(input))
  logical,intent(in) :: doubled
  integer :: n
  logical :: visited2(size(input))
  logical :: doubled2
  doubled2 = doubled
  if (cur == last) then
    paths = paths + 1
    return
  end if
  if (visited(cur)) then
    if (doubled) then
      return
    else
      if (cur == root) return
      doubled2 = .true.
    end if
  end if
  visited2 = visited
  if (.not.capital(cur)) visited2(cur) = .true.
  do n = 1,map_size
    if (links(cur,n)) then
      call walk2(n,paths,visited2, doubled2)
    end if
  end do

end subroutine


end subroutine
