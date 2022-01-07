subroutine solver19(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer        :: part, n, o, t, x, y, z, mm, nn, mmm, nnn, cnt, dist
  type(char_p)   :: input(:)
  type scanner
    integer, pointer :: beacons(:,:)
    integer, pointer :: view(:,:)
    integer          :: offset(3) = 0
  end type
  integer  :: done_n, prev_done, cur_done, beacons(3,size(input)), beacons_n
  type(scanner), allocatable :: scanners(:), tmp(:), done(:)
  integer, pointer :: btmp(:,:)
  integer, pointer :: view(:,:)
  integer :: offset(3)
  logical, allocatable :: mask(:)
  allocate(scanners(0))

  do n=1,size(input)
    if (len(input(n)%p) == 0) cycle
    if (input(n)%p(2:2) == "-") then
      allocate(tmp(size(scanners)+1))
      tmp(:size(scanners)) = scanners
      deallocate(scanners)
      call move_alloc(tmp, scanners)
      allocate(scanners(size(scanners))%beacons(3,0))
      cycle
    end if

    allocate(btmp(1:3,size(scanners(size(scanners))%beacons)/3+1))
    btmp(:,:size(scanners(size(scanners))%beacons,2)) = scanners(size(scanners))%beacons
    deallocate(scanners(size(scanners))%beacons)
    read(input(n)%p,*) btmp(1:3,size(btmp,2))
    scanners(size(scanners))%beacons => btmp
  end do

  do n = 1, size(scanners)
    allocate(scanners(n)%view(3,size(scanners(n)%beacons,2)))
  end do


  allocate(done(size(scanners)))
  done_n = 1
  done(1) = scanners(1)

  allocate(mask(size(scanners)))
  mask = .true.
  mask(1) = .false.

  view => null()
  prev_done = 1 
  do while (any(mask))
    cur_done = done_n
    do n=1,size(scanners)
      if (.not.mask(n)) cycle
      if (.not.associated(scanners(n)%view)) then
        write(*,*) "Wtf"
      end if
      view => scanners(n)%view
      do t=1,6
        do x=-1,1,2
          do y=-1,1,2
            do z=-1,1,2
              call rotate_and_flip(scanners(n),t, x, y, z, view)
              do o = prev_done,cur_done
                do nn = 1, size(view,2) - 12
                  do mm = 1, size(done(o)%beacons,2)
                    cnt = 1
                    offset = done(o)%beacons(:,mm) - view(:,nn) 
                    do nnn = nn + 1, size(view,2)
                      do mmm = 1, size(done(o)%beacons,2)
                        if (all(offset == done(o)%beacons(:,mmm) - view(:,nnn))) then
                          cnt = cnt + 1
                          if (cnt == 12) then
                            done_n = done_n + 1
                            done(done_n)%offset = offset + done(o)%offset
                            done(done_n)%beacons => view
                            mask(n) = .false.
                            goto 3
                          end if
                        end if
                      end do
                    end do
                  end do
                end do
              end do
            end do
          end do
        end do
      end do
3     continue
    end do
    prev_done = cur_done + 1
  end do
  
  if (part == 1) then

    beacons_n = 0
    cnt = 0

    do n = 1, done_n
      offset = done(n)%offset
      do nn = 1, size(done(n)%beacons,2)
        cnt = cnt + 1
        do nnn = 1, beacons_n
          if(all(beacons(:,nnn) == done(n)%beacons(:,nn) + offset)) goto 4
        end do

        beacons_n = beacons_n + 1
        beacons(:,beacons_n) = done(n)%beacons(:,nn) + offset
  4   end do
    end do
    ans = beacons_n 
  else
    dist = 0
    do n = 1, done_n - 1
      do nn = n + 1, done_n
        dist = max(dist, sum(abs(done(n)%offset - done(nn)%offset)))
      end do
    end do
    ans = dist
  end if

contains 
subroutine rotate_and_flip(input, t, x, y, z, view)
  implicit none
  type(scanner) :: input
  integer :: t, x,y,z
  integer, pointer :: view(:,:)
  if (t == 1) then
    view = input%beacons
  else if (t == 2) then
    view(1,:) =  input%beacons(1,:)
    view(2,:) =  input%beacons(3,:)
    view(3,:) =  input%beacons(2,:)
  else if (t == 3) then
    view(1,:) =  input%beacons(3,:)
    view(2,:) =  input%beacons(2,:)
    view(3,:) =  input%beacons(1,:)
  else if (t == 4) then
    view(1,:) =  input%beacons(2,:)
    view(2,:) =  input%beacons(1,:)
    view(3,:) =  input%beacons(3,:)
  else if (t == 5) then
    view(1,:) =  input%beacons(2,:)
    view(2,:) =  input%beacons(3,:)
    view(3,:) =  input%beacons(1,:)
  else if (t == 6) then
    view(1,:) =  input%beacons(3,:)
    view(2,:) =  input%beacons(1,:)
    view(3,:) =  input%beacons(2,:)
  end if

  view(1,:) = view(1,:) * x
  view(2,:) = view(2,:) * y
  view(3,:) = view(3,:) * z
end subroutine
              
end subroutine
