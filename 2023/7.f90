subroutine solver7(part,input,ans)
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
  type, bind(c) :: hand
    integer(c_int32_t) :: typ
    integer(c_int32_t) :: cards(5)
    integer(c_int32_t) :: bid
  end type
  type(hand), target :: hands(size(input))
  integer :: n, m

  do n =1,size(input)
    p => input(n)%p
    do m = 1, 5
      select case(p(m:m))
        case('T')
          hands(n)%cards(m) = 10
        case('J')
          hands(n)%cards(m) = 11
        case('Q')
          hands(n)%cards(m) = 12
        case('K')
          hands(n)%cards(m) = 13
        case('A')
          hands(n)%cards(m) = 14
        case default
          read(p(m:m),*) hands(n)%cards(m)
      end select
    end do
    hands(n)%typ = get_type(hands(n)%cards)
    read(p(7:),*) hands(n)%bid
  end do

  call qsort(c_loc(hands),int(size(input),c_size_t) &
                ,c_sizeof(hands(1)),c_funloc(compare))

  do n = 1, size(hands)
    ans = ans + hands(n)%bid*n
  end do
end subroutine
function get_type(cards)
  implicit none
  integer :: cards(5), counts(14), get_type
  integer :: n
  counts = 0
  get_type = 0
  do n = 1, 5
    counts(cards(n)) = counts(cards(n)) + 1
  end do
  if (any(counts==5)) then
    get_type = 6
    return
  end if
  if (any(counts==4)) then
    get_type = 5
    return
  end if
  if (any(counts==3) .and. any(counts==2)) then
    get_type = 4
    return
  end if
  if (any(counts==3)) then
    get_type = 3
    return
  end if
  get_type = count(counts==2)
end function
function compare(a,b)
  type, bind(c) :: hand
    integer(c_int32_t) :: typ
    integer(c_int32_t) :: cards(5)
    integer(c_int32_t) :: bid
  end type
  type(hand) :: a,b
  integer(2) :: compare
  integer :: n
  if (a%typ > b%typ) then
    compare = 1
    return
  end if
  if (a%typ < b%typ) then
    compare = -1
    return
  end if
  do n = 1, 5
    if ( a%cards(n) .lt. b%cards(n) ) then 
      compare = -1
      return
    end if
    if ( a%cards(n) .gt. b%cards(n) ) then 
      compare = 1
      return
    end if
  end do
  compare = 0 
end function
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  type, bind(c) :: hand
    integer(c_int32_t) :: typ
    integer(c_int32_t) :: cards(5)
    integer(c_int32_t) :: bid
  end type
  type(hand), target :: hands(size(input))
  integer :: n, m

  do n =1,size(input)
    p => input(n)%p
    do m = 1, 5
      select case(p(m:m))
        case('T')
          hands(n)%cards(m) = 10
        case('J')
          hands(n)%cards(m) =  1
        case('Q')
          hands(n)%cards(m) = 12
        case('K')
          hands(n)%cards(m) = 13
        case('A')
          hands(n)%cards(m) = 14
        case default
          read(p(m:m),*) hands(n)%cards(m)
      end select
    end do
    hands(n)%typ = get_type2(hands(n)%cards)
    read(p(7:),*) hands(n)%bid
  end do

  call qsort(c_loc(hands),int(size(input),c_size_t) &
                ,c_sizeof(hands(1)),c_funloc(compare))

  do n = 1, size(hands)
    ans = ans + hands(n)%bid*n
  end do
end subroutine
function get_type2(cards)
  implicit none
  integer :: cards(5), counts(14), ccounts(14), get_type2
  integer :: n
  counts = 0
  get_type2 = 0
  ccounts = 0
  do n = 1, 5
    if (cards(n) == 1) then
      counts = counts + 1
    else
      counts(cards(n)) = counts(cards(n)) + 1
    end if
  end do
  do n = 1, 5
    ccounts(cards(n)) = ccounts(cards(n)) + 1
  end do
  if (any(counts==5)) then
    get_type2 = 6
    return
  end if
  if (any(counts==4)) then
    get_type2 = 5
    return
  end if
  if (count(ccounts>1)==2 .and. (any(ccounts==3) .or. ccounts(1) ==1)) then
    get_type2 = 4
    return
  end if
  if (any(counts==3)) then
    get_type2 = 3
    return
  end if
  if (count(ccounts==2)==2) then
    get_type2 = 2
    return
  end if
  if (any(counts==2)) then
    get_type2 = 1
    return
  end if
  get_type2 = 0
end function
end subroutine
