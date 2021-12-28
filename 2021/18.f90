subroutine solver18(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n,nn, nextnum
  integer(int64) :: newmag
  type(char_p)   :: input(:)
  type snail_n
    integer a,b
    type(snail_n), pointer :: aa => null()
    type(snail_n), pointer :: bb => null()
  end type
  type(snail_n), pointer :: root
  type(snail_n), pointer :: buf
  integer, pointer :: lastnum => null()
  logical :: exploded, splitted

  if (part == 2) goto 2
  allocate(root)

  call read_snail_n(input(1)%p, root)

  do n=2,size(input)
    buf => root
    allocate(root)
    root%aa => buf
    allocate(root%bb)
    call read_snail_n(input(n)%p, root%bb)

    do
      exploded = .false.
      lastnum => null()
      nextnum = -1
      call explode(root,0, lastnum, nextnum, exploded)
      if (exploded) goto 3
      call split(root, splitted)
      if (splitted) goto 3
      exit
3   end do
  end do

  ans = score_snail(root)
  return

2 continue
  ans = 0
  do n=1,size(input)
    do nn=1, size(input)
      if (n == nn) cycle
      allocate(root)
      allocate(root%bb)
      allocate(root%aa)
      call read_snail_n(input(n)%p, root%aa)
      call read_snail_n(input(nn)%p, root%bb)

      do
        exploded = .false.
        lastnum => null()
        nextnum = -1
        call explode(root,0, lastnum, nextnum, exploded)
        if (exploded) goto 4
        call split(root, splitted)
        if (splitted) goto 4
        exit
4     end do
      newmag = score_snail(root)
      if (newmag > ans) then
        ans = newmag
      end if

      deallocate(root) ! I know, but fuck it
    end do
  end do

          
contains

recursive function score_snail(snail) result(score)
  type(snail_n) :: snail
  integer(int64) :: score
  if (associated(snail%aa)) then
    score = 3*score_snail(snail%aa)
  else
    score = 3*snail%a 
  end if
  if (associated(snail%bb)) then
    score = score + 2*score_snail(snail%bb)
  else
    score = score + 2*snail%b 
  end if
end function

recursive subroutine write_snail(snail)
  type(snail_n) :: snail
  write(*,'(A1)',advance="no") "["
  if (associated(snail%aa)) then
    call write_snail(snail%aa)
  else
    write(*,'(I2)',advance="no") snail%a 
  end if
  write(*,'(A1)', advance="no") ","
  if (associated(snail%bb)) then
    call write_snail(snail%bb)
  else
    write(*,'(I2)',advance="no") snail%b
  end if
  write(*,'(A1)',advance="no") "]"
end subroutine


recursive subroutine read_snail_n(input, snail)
  type(snail_n), pointer, intent(inout) :: snail
  character(len=*), intent(in) :: input
  integer :: n, level
  if (ichar(input(2:2)) < 58) then
    read(input(2:2),'(I1)') snail%a
    if (input(3:3) == ",") then
      if (ichar(input(4:4)) < 58 ) then
        read(input(4:4),'(I1)') snail%b
        return
      else
        allocate(snail%bb)
        call read_snail_n(input(4:len(input)-1), snail%bb)
      end if
    end if
  else
    level = 1
    do n = 3, len(input)
      if (input(n:n) == "[") level = level + 1
      if (input(n:n) == "]") level = level - 1
      if (level == 0) then
        allocate(snail%aa)
        call read_snail_n(input(2:n), snail%aa)
        exit
      end if
    end do
    if (ichar(input(n+2:n+2)) < 58 ) then
      read(input(n+2:),'(I1)') snail%b
      return
    else
      allocate(snail%bb)
      call read_snail_n(input(n+2:len(input)-1), snail%bb)
    end if
  end if
      

end subroutine

recursive subroutine explode(snail, level, lastnum, nextnum, exploded)
  type(snail_n), intent(inout), pointer :: snail
  integer, intent(in)    :: level
  logical, intent(inout) :: exploded
  integer, pointer, intent(inout) :: lastnum
  integer, intent(inout) :: nextnum

  if (exploded.and.(nextnum == -1)) return

  if (exploded) then
    if (associated(snail%aa)) then
      call explode(snail%aa,level+1,lastnum, nextnum, exploded)
    else
      snail%a = snail%a + nextnum
      nextnum = -1
      return
    end if
    if (nextnum == -1) return
    if (associated(snail%bb)) then
      call explode(snail%bb, level+1,lastnum,nextnum, exploded)
    else
      snail%b = snail%b + nextnum
      nextnum = -1
      return
    end if
    return
  end if

  if (level == 3 .and. .not. exploded) then
    if (associated(snail%aa)) then
      if (associated(lastnum)) then
        lastnum = lastnum + snail%aa%a
      end if
      if (associated(snail%bb)) then
        snail%bb%a = snail%bb%a + snail%aa%b
      else
        snail%b = snail%b + snail%aa%b
      end if
      deallocate(snail%aa)
      snail%aa => null()
      snail%a = 0
      exploded = .true.
      return
    end if
    if (associated(snail%bb)) then
      snail%a = snail%a + snail%bb%a
      nextnum = snail%bb%b
      deallocate(snail%bb)
      snail%bb => null()
      snail%b = 0
      exploded = .true.
      return
    end if
  end if

  if (associated(snail%aa)) then
    call explode(snail%aa,level+1,lastnum,nextnum,exploded)
  else
    lastnum => snail%a
  end if
  if (exploded.and.(nextnum == -1)) return
  if (associated(snail%bb)) then
    call explode(snail%bb, level+1,lastnum,nextnum,exploded)
  else
    lastnum => snail%b
    if (exploded) then
      snail%b = snail%b + nextnum
      nextnum = -1
    end if
  end if
  return 

end subroutine

recursive subroutine split(snail, splitted)
  type(snail_n), intent(inout), pointer :: snail
  logical, intent(out) :: splitted
  splitted = .false.

  if (associated(snail%aa)) then
    call split(snail%aa,splitted)
  else
    if (snail%a > 9 ) then
      allocate(snail%aa)
      snail%aa%a = snail%a / 2
      snail%aa%b = ( snail%a / 2 ) + mod(snail%a,2)
      splitted = .true.
    end if
  end if
  if (splitted) return
  if (associated(snail%bb)) then
    call split(snail%bb,splitted)
  else
    if (snail%b > 9 ) then
      allocate(snail%bb)
      snail%bb%a = snail%b / 2
      snail%bb%b = ( snail%b / 2 ) + mod(snail%b,2)
      splitted = .true.
    end if
  end if
  return 
end subroutine
end subroutine
