subroutine solver16(part,input,ans)
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
  character, allocatable :: contraption(:,:)
  integer :: n, m
  
  allocate(contraption(len(input(1)%p),size(input)))

  do n =1,size(input)
    p => input(n)%p
    do m = 1, len(p)
      contraption(m,n) = p(m:m)
    end do
  end do

  ans = solve(0,1,0, contraption)

end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  character, allocatable :: contraption(:,:)
  integer :: n, m 
  
  allocate(contraption(len(input(1)%p),size(input)))

  do n =1,size(input)
    p => input(n)%p
    do m = 1, len(p)
      contraption(m,n) = p(m:m)
    end do
  end do
  
  do n = 1,size(contraption,2) 
    ans = max(ans,solve(0,n,0, contraption))
    ans = max(ans,solve(size(contraption,1)+1,n,2, contraption))
  end do
  do n = 1,size(contraption,1) 
    ans = max(ans,solve(n,0,1, contraption))
    ans = max(ans,solve(n,size(contraption,2)+1,3, contraption))
  end do
end subroutine
function solve(startx,starty,dir,contraption) result(ans)
  implicit none
  integer(int64) :: ans
  integer :: startx, starty, dir
  type lazor_bean
    integer x,y
    integer dir
  end type
  type(lazor_bean), pointer :: lazor
  type(lazor_bean), target :: lazors(100)
  integer :: n, m, l, nlazors
  integer, allocatable :: cavern(:,:), cavern_check(:,:)
  character :: contraption(:,:), c
  nlazors = 1
  lazors(1) = lazor_bean(x=startx,y=starty,dir=dir)

  allocate(cavern(len(input(1)%p),size(input)))
  allocate(cavern_check(len(input(1)%p),size(input)))
  cavern = 0
  cavern_check = 2

  do while (.not. all(cavern == cavern_check))
    cavern_check = cavern

    ! move
    n = 1
    do while (n <= nlazors)
      lazor => lazors(n)
      select case(lazor%dir)
      case(0) 
        lazor%x = lazor%x + 1
      case(1) 
        lazor%y = lazor%y + 1
      case(2) 
        lazor%x = lazor%x - 1
      case(3) 
        lazor%y = lazor%y - 1
      end select

    ! bounds check
      if (lazor%x < 1 .or. lazor%y < 1 .or. lazor%x > size(cavern, 1) &
          .or. lazor%y > size(cavern,2)) then
        lazor = lazors(nlazors)
        nlazors = nlazors - 1
        cycle
      end if

    ! Paint path
      l = cavern(lazor%x,lazor%y)
      m = ior(l,2**lazor%dir) 
      if (m == l) then
        lazor = lazors(nlazors)
        nlazors = nlazors - 1
        cycle
      else
        cavern(lazor%x,lazor%y) = m
      end if

    ! Specials
      c = contraption(lazor%x,lazor%y) 
      select case(c)
      case('/')
        select case(lazor%dir)
        case(0)
          lazor%dir = 3
        case(1)
          lazor%dir = 2
        case(2)
          lazor%dir = 1
        case(3)
          lazor%dir = 0
        end select
      case('\')
        select case(lazor%dir)
        case(0)
          lazor%dir = 1
        case(1)
          lazor%dir = 0
        case(2)
          lazor%dir = 3
        case(3)
          lazor%dir = 2
        end select
      case('|')
        if(lazor%dir == 0 .or. lazor%dir == 2) then
          lazor%dir = 1
          nlazors = nlazors+1
          lazors(nlazors) = lazor
          lazors(nlazors)%dir = 3
        end if
      case('-')
        if(lazor%dir == 1 .or. lazor%dir == 3) then
          lazor%dir = 0
          nlazors = nlazors+1
          lazors(nlazors) = lazor
          lazors(nlazors)%dir = 2
        end if
      end select
      n = n + 1
    end do
  end do
  ans = count(cavern /=0)
  
end function
end subroutine
