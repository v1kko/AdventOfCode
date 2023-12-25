subroutine solver14(part,input,ans)
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
  character, allocatable :: platform(:,:)
  character, allocatable :: state(:)
  integer :: n, m

  allocate(platform(size(input),len(input(1)%p)))
  allocate(state(size(input)))
  do n =1,size(platform,1)
    do m = 1, size(platform,2)
      ! Transpose for cache efficiency
      platform(n,m) = input(n)%p(m:m)
    end do
  end do
  do n = 1, size(platform,2)
    do
      state = platform(:,n)
      do m = 1, size(platform,1) -1
        if (all(platform(m:m+1,n) == (/'.','O'/))) platform(m:m+1,n) = (/"O","."/)
      end do
      if (all(state == platform(:,n))) exit
    end do
    do m = 1, size(platform,1)
      if (platform(m,n) == 'O') ans = ans + size(platform,1) - m + 1
    end do
  end do

end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character, allocatable :: platform(:,:)
  character, allocatable :: state(:)
  integer(int64) :: res(500)
  integer :: n, m, s

  allocate(platform(len(input(1)%p),size(input)))
  allocate(state(size(input)))
  do n =1,size(platform,2)
    do m = 1, size(platform,1)
      platform(m,n) = input(n)%p(m:m)
    end do
  end do
  do n = 1, size(platform,2)
    do m = 1, size(platform,1)
      if (platform(m,n) == 'O') ans = ans + size(platform,2) - n + 1
    end do
  end do
  outer: do s = 1, 500
    ! North
    do n = 1, size(platform,1)
      do
        state = platform(n,:)
        do m = 1, size(platform,2) -1
          if (all(platform(n,m:m+1) == (/'.','O'/))) then
            platform(n,m:m+1) = (/"O","."/)
            ans = ans + 1
          end if
        end do
        if (all(state == platform(n,:))) exit
      end do
    end do
    ! West
    do n = 1, size(platform,2)
      do
        state = platform(:,n)
        do m = 1, size(platform,1) -1
          if (all(platform(m:m+1,n) == (/'.','O'/))) platform(m:m+1,n) = (/"O","."/)
        end do
        if (all(state == platform(:,n))) exit
      end do
    end do
    ! South
    do n = 1, size(platform,1)
      do
        state = platform(n,:)
        do m = size(platform,2), 2, -1
          if (all(platform(n,m-1:m) == (/'O','.'/))) then
            platform(n,m-1:m) = (/".","O"/)
            ans = ans - 1
          end if
        end do
        if (all(state == platform(n,:))) exit
      end do
    end do
    ! East
    do n = 1, size(platform,2)
      do
        state = platform(:,n)
        do m = size(platform,1), 2, -1
          if (all(platform(m-1:m,n) == (/'O','.'/))) platform(m-1:m,n) = (/".","O"/)
        end do
        if (all(state == platform(:,n))) exit
      end do
    end do

    res(s) = ans
    do n = s-1,s/2+1,-1
      if (res(n) == ans) then
        if (n == s-1) cycle outer
        if (mod(1000000000-s,s-n) /= 0) cycle outer
        if (.not. all(res(n-(s-n)+1:n) == res(n+1:s))) cycle outer
        return
      end if
    end do
    !end if
  end do outer
end subroutine
end subroutine
