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
    read(p(7:),*) hands(n)%bid
  end do

  call qsort(c_loc(hands),int(size(input),c_size_t) &
                ,c_sizeof(hands(1)),c_funloc(compare))

  do n = 1, size(hands)
    ans = ans + hands(n)%bid*n
  end do
end subroutine
function compare(a,b)
  type, bind(c) :: hand
    integer(c_int32_t) :: cards(5)
    integer(c_int32_t) :: bid
  end type
  type(hand) :: a,b
  integer(2) :: compare
  integer :: n
  integer :: acounts(14), bcounts(14)
  acounts = 0
  bcounts = 0
  do n = 1, 5
    acounts(a%cards(n)) = acounts(a%cards(n)) + 1
    bcounts(b%cards(n)) = bcounts(b%cards(n)) + 1
  end do
  
  if (any(acounts==5) .and. .not.any(bcounts==5)) then
    compare = 1
    return
  end if
  if (any(bcounts==5) .and. .not.any(acounts==5)) then
    compare = -1
    return
  end if

  if (any(acounts==4) .and. .not.any(bcounts==4)) then
    compare = 1
    return
  end if
  if (any(bcounts==4) .and. .not.any(acounts==4)) then
    compare = -1
    return
  end if

  if (any(acounts==3) .and. any(acounts==2) &
      .and. .not.(any(bcounts==3).and.any(bcounts==2))) then
    compare = 1
    return
  end if
  if (any(bcounts==3) .and. any(bcounts==2) &
      .and. .not.(any(acounts==3).and.any(acounts==2))) then
    compare = -1
    return
  end if

  if (any(acounts==3) .and. .not.any(bcounts==3)) then
    compare = 1
    return
  end if
  if (any(bcounts==3) .and. .not.any(acounts==3)) then
    compare = -1
    return
  end if

  if (count(acounts==2)==2 .and. .not.count(bcounts==2)==2) then
    compare = 1
    return
  end if
  if (count(bcounts==2)==2 .and. .not.count(acounts==2)==2) then
    compare = -1
    return
  end if

  if (any(acounts==2) .and. .not.any(bcounts==2)) then
    compare = 1
    return
  end if
  if (any(bcounts==2) .and. .not.any(acounts==2)) then
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
    read(p(7:),*) hands(n)%bid
  end do

  call qsort(c_loc(hands),int(size(input),c_size_t) &
                ,c_sizeof(hands(1)),c_funloc(compare2))

  do n = 1, size(hands)
    ans = ans + hands(n)%bid*n
  end do
end subroutine
function compare2(a,b) result(compare)
  type, bind(c) :: hand
    integer(c_int32_t) :: cards(5)
    integer(c_int32_t) :: bid
  end type
  type(hand) :: a,b
  integer(2) :: compare
  integer :: n
  integer :: acounts(14), bcounts(14)
  integer :: aacounts(14), bbcounts(14)
  acounts = 0
  bcounts = 0
  do n = 1, 5
    if (a%cards(n) == 1) then
      acounts = acounts + 1
    else
      acounts(a%cards(n)) = acounts(a%cards(n)) + 1
    end if
    if (b%cards(n) == 1) then
      bcounts = bcounts + 1
    else
      bcounts(b%cards(n)) = bcounts(b%cards(n)) + 1
    end if
  end do
  aacounts = 0
  bbcounts = 0
  do n = 1, 5
    aacounts(a%cards(n)) = aacounts(a%cards(n)) + 1
    bbcounts(b%cards(n)) = bbcounts(b%cards(n)) + 1
  end do
  
  if (any(acounts==5) .and. .not.any(bcounts==5)) then
    compare = 1
    return
  end if
  if (any(bcounts==5) .and. .not.any(acounts==5)) then
    compare = -1
    return
  end if
  if (any(bcounts==5) .or. any(acounts==5)) goto 5

  if (any(acounts==4) .and. .not.any(bcounts==4)) then
    compare = 1
    return
  end if
  if (any(bcounts==4) .and. .not.any(acounts==4)) then
    compare = -1
    return
  end if
  if (any(bcounts==4) .or. any(acounts==4)) goto 5

  ! TODO Fix from here
  if        ((count(aacounts>1)==2 .and. (any(aacounts==3) .or. aacounts(1) ==1)) &
  .and. .not.(count(bbcounts>1)==2 .and. (any(bbcounts==3) .or. bbcounts(1) ==1))) then
    compare = 1
    return
  end if
  if        ((count(bbcounts>1)==2 .and. (any(bbcounts==3) .or. bbcounts(1) ==1)) &
  .and. .not.(count(aacounts>1)==2 .and. (any(aacounts==3) .or. aacounts(1) ==1))) then
    compare = -1
    return
  end if
  if        ((count(bbcounts>1)==2 .and. (any(bbcounts==3) .or. bbcounts(1) ==1)) &
  .or.       (count(aacounts>1)==2 .and. (any(aacounts==3) .or. aacounts(1) ==1))) goto 5

  if (any(acounts==3) .and. .not.any(bcounts==3)) then
    compare = 1
    return
  end if
  if (any(bcounts==3) .and. .not.any(acounts==3)) then
    compare = -1
    return
  end if
  if (any(bcounts==3) .or. any(acounts==3)) goto 5


  if        ((count(aacounts==2)==2 .or. (aacounts(1) == 1 .and. any(aacounts==2))) &
  .and. .not.(count(bbcounts==2)==2 .or. (bbcounts(1) == 1 .and. any(bbcounts==2)))) then
    compare = 1
    return
  end if
  if        ((count(bbcounts==2)==2 .or. (bbcounts(1) == 1 .and. any(bbcounts==2))) &
  .and. .not.(count(aacounts==2)==2 .or. (aacounts(1) == 1 .and. any(aacounts==2)))) then
    compare = -1
    return
  end if

  if (any(acounts==2) .and. .not.any(bcounts==2)) then
    compare = 1
    return
  end if
  if (any(bcounts==2) .and. .not.any(acounts==2)) then
    compare = -1
    return
  end if

5 do n = 1, 5
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
end subroutine
