subroutine solver5(part,input,ans)
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
  integer :: n, s, d, nseeds = 0
  integer(int64) :: dd, ss, rr
  integer(int64), allocatable :: seeds(:)
  integer(int64), allocatable :: newseeds(:)
  n = 1

  ! Read in seeds
  p => input(n)%p
  allocate(seeds(len(p)))
  s = scan(p," ")+1
  DO
    d = scan(p(s:)," ")
    if (d == 0) then
      d = len(p)
    else
      d = d + s - 2
    end if
    nseeds = nseeds + 1
    read(p(s:d),*) seeds(nseeds)
    s = d + 2
    if (s > len(p)) exit
  END DO
  allocate(newseeds(nseeds))
  newseeds=seeds(:nseeds)
  call move_alloc(newseeds,seeds)
  allocate(newseeds(nseeds))


  do while(n < size(input))
    n = n + 1
    p => input(n)%p
    s = scan(p,":")
    if (s == 0) cycle

    newseeds = seeds
    do while (n < size(input))
      n = n + 1
      p => input(n)%p
      if (p == "") exit
      read(p,*) dd, ss, rr
      where (ss<=seeds.and.seeds<=ss+rr) 
        newseeds = seeds-ss+dd
      end where
    end do
    seeds = newseeds
  end do
  ans = minval(seeds)
  deallocate(seeds, newseeds)
end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  integer :: n, m, s, d,nseeds = 0, nnewseeds
  integer(int64) :: dd, ss, rr, sr, dr
  integer(int64) :: seeds(2,200), newseeds(2,200), flatseeds(400)
  seeds = 0
  n = 1

  ! Read in seeds
  p => input(n)%p
  s = scan(p," ")+1
  do
    d = scan(p(s:)," ")
    if (d == 0) then
      d = len(p)
    else
      d = d + s - 2
    end if
    nseeds = nseeds + 1
    read(p(s:d),*) flatseeds(nseeds)
    s = d + 2
    if (s > len(p)) exit
  end do
  seeds(:,:nseeds/2) = reshape(flatseeds(:nseeds),(/2,nseeds/2/))
  nseeds = nseeds/2
  seeds(2,:nseeds) = seeds(1,:nseeds) + seeds(2,:nseeds) - 1

  do while(n < size(input))
    n = n + 1
    p => input(n)%p
    s = scan(p,":")
    if (s == 0) cycle

    nnewseeds = 0
    do while (n < size(input))
      n = n + 1
      p => input(n)%p
      if (p == "") exit
      read(p,*) dd, ss, rr
      rr = ss + rr - 1
      m = 0
      do while(m < nseeds)
        m = m + 1
        sr = max(ss, seeds(1,m))
        dr = min(rr, seeds(2,m))
        if (sr > dr) cycle
        if (sr /= seeds(1,m)) then
          nseeds = nseeds + 1
          seeds(:,nseeds) = (/seeds(1,m),sr-1/) 
        end if
        if (dr /= seeds(2,m)) then
          nseeds = nseeds + 1
          seeds(:,nseeds) = (/dr+1,seeds(2,m)/) 
        end if

        nnewseeds = nnewseeds + 1
        newseeds(:,nnewseeds) = (/sr+dd-ss,dr+dd-ss/)

        seeds(:,m) = seeds(:,nseeds)
        nseeds = nseeds - 1
        m = 0
      end do
    end do
    newseeds(:,nnewseeds+1:nnewseeds+nseeds) = seeds(:,:nseeds)
    nseeds = nnewseeds + nseeds
    seeds(:,:nseeds) = newseeds(:,:nseeds)
  end do
  ans = minval(seeds(:,:nseeds))
end subroutine
end subroutine
