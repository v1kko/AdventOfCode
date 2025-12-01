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
  integer :: n, m, x

  allocate(platform(size(input),len(input(1)%p)))
  allocate(state(size(input)))
  do n =1,size(platform,1)
    do m = 1, size(platform,2)
      ! Transpose for cache efficiency
      platform(n,m) = input(n)%p(m:m)
    end do
  end do
  do n = 1, size(platform,2)
    do m = 2, size(platform,1)
      if (platform(m,n) == 'O') then
        platform(m,n) = '.'
        do x = m-1,1,-1
          if (platform(x,n) /= '.') then
            platform(x+1,n) = 'O'
            exit
          end if
        end do
        if (x==0) then
          platform(1,n) = 'O'
        end if
      end if
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
  character, allocatable :: platform(:,:), cache(:,:,:)
  integer(int64) :: res(200)
  integer :: n, m, s, x

  allocate(platform(len(input(1)%p),size(input)))
  allocate(cache(size(platform,1),size(platform,2),200))
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

  outer: do s = 1, 200
    ! North
    do n = 1, size(platform,1)
      do m = 2, size(platform,2)
        if (platform(n,m) == 'O') then
          platform(n,m) = '.'
          do x = m-1,1,-1
            if (platform(n,x) /= '.') then
              platform(n,x+1) = 'O'
              exit
            end if
          end do
          if (x==0) then
            platform(n,1) = 'O'
          end if
          ans = ans + (m-(x+1))
        end if
      end do
    end do
    ! West
    do n = 1, size(platform,2)
      do m = 2, size(platform,1)
        if (platform(m,n) == 'O') then
          platform(m,n) = '.'
          do x = m-1,1,-1
            if (platform(x,n) /= '.') then
              platform(x+1,n) = 'O'
              exit
            end if
          end do
          if (x==0) then
            platform(1,n) = 'O'
          end if
        end if
      end do
    end do
    ! South
    do n = 1, size(platform,1)
      do m = size(platform,2)-1, 1, -1
        if (platform(n,m) == 'O') then
          platform(n,m) = '.'
          do x = m+1,size(platform,2)
            if (platform(n,x) /= '.') then
              platform(n,x-1) = 'O'
              exit
            end if
          end do
          if (x==size(platform,2)+1) then
            platform(n,size(platform,2)) = 'O'
          end if
          ans = ans - ((x-1)-m)
        end if
      end do
    end do
    ! East
    do n = 1, size(platform,2)
      do m = size(platform,1)-1, 1, -1
        if (platform(m,n) == 'O') then
          platform(m,n) = '.'
          do x = m+1,size(platform,1)
            if (platform(x,n) /= '.') then
              platform(x-1,n) = 'O'
              exit
            end if
          end do
          if (x==size(platform,1)+1) then
            platform(size(platform,1),n) = 'O'
          end if
        end if
      end do
    end do

    cache(:,:,s) = platform
    res(s) = ans

    do n = s-1,1,-1
      if (ans == res(n)) then
        if (all(cache(:,:,n) == platform)) then
          x = mod(1000000000-s,s-n)
          ans = res(n + x)
          return
        end if
      end if
    end do
    !end if
  end do outer
end subroutine
end subroutine
