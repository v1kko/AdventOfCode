subroutine solver16(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans, val
  integer        :: part, n, v_sum, dum
  integer*1      :: hex
  type(char_p)   :: input(:)
  character(len=:), allocatable, target :: in_b
  character(len=4) :: hex_s
  in_b = ''

  do n = 1, len(input(1)%p)
    read(input(1)%p(n:n),'(Z1)') hex
    write(hex_s,'(B4.4)') hex
    in_b = in_b // hex_s
  end do
  v_sum = 0
  call parser(in_b, dum, val)
  if (part == 1) then
    ans = v_sum
  else
    ans = val
  end if

contains 
recursive subroutine parser(input, last, val)
    implicit none
    character(len=*) :: input
    integer(int64), intent(out) :: val
    integer, intent(out) :: last
    integer          :: version
    character(len=3) :: type

    read(input(1:3),'(B3)') version 
    v_sum = v_sum + version
    type = input(4:6)
    if (type == '100') then
      call type4(input(7:len(input)), last, val)
    else
      call op(input(7:len(input)), last, val, type)
    end if
    last = last + 6
    return
end subroutine

subroutine type4(input,last,val)
  implicit none
  character(len=*) :: input
  integer,intent(out)     :: last
  integer(int64), intent(out)    :: val
  character(len=80) :: bufl
  character(len=:), allocatable :: number
  character(len=4) :: buf
  integer:: n 
  n = 1
  val = 0
  number = ""
  do while(input(n:n) == "1")
    read(input(n+1:n+4),'(A4)') buf
    number = number // buf 
    n = n + 5
  end do
  read(input(n+1:n+4),'(A4)') buf
  number = number // buf 
  last =  n + 5
  write(bufl,*) len(number)
  read(number,'(B'//bufl//')') val
  return
end subroutine

recursive subroutine op(input, last, val, type)
  implicit none
  character(len=*) :: input
  character(len=3) :: type
  integer(int64), intent(out) :: val
  integer :: vals_i
  integer(int64) :: vals(100)
  integer :: contained, last, cur, n
  vals_i = 0
  if (input(1:1) == "0") then
    read(input(2:16),'(B15)') contained
    cur = 17
    do while (cur <= contained+16)
      vals_i = vals_i + 1
      call parser(input(cur:contained+16), last, vals(vals_i))
      cur = cur + last - 1
    end do
    last = 17 + contained 
  else
    read(input(2:12),'(B11)') contained
    cur = 13
    do n = 1,contained
      vals_i = vals_i + 1
      call parser(input(cur:len(input)),last, vals(vals_i))
      cur = cur + last - 1
    end do
    last =cur 
  end if
  val = 1
  if (type == "000") val = sum(vals(:vals_i)) 
  if (type == "001") then
    do n = 1,vals_i
      val = val * vals(n)
    end do
  end if
  if (type == "010") val = minval(vals(:vals_i))
  if (type == "011") val = maxval(vals(:vals_i))
  if (type == "101") then
    if (vals(1) <= vals(2)) val = 0
  end if
  if (type == "110") then
    if (vals(1) >= vals(2)) val = 0
  end if
  if (type == "111") then
    if (vals(1) /= vals(2)) val = 0
  end if
    
  return
end subroutine



end subroutine
