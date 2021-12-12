subroutine solver4(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64) :: ans
  integer :: part, n, nn, n_boards, draw, next_c, prev_c
  type(char_p) :: input(:)
  integer :: boards(5,5,size(input)/6)
  logical :: masks(5,5,size(input)/6)
  logical :: won(size(input)/6)

  n_boards = size(input)/6
  masks = .true.

  do n=1,n_boards
    do nn=1,5
      read(input(2+(n-1)*6+nn)%p,*) boards(:,nn,n)
    end do
  end do

  prev_c = 1

  if (part == 2) goto 2

  do
    next_c = prev_c + index(input(1)%p(prev_c+1:),",")
    if (next_c .eq. prev_c) exit
    read(input(1)%p(prev_c:next_c),*) draw
    prev_c = next_c + 1

    where (boards == draw)
      masks = .false.
    end where

    do n = 1,n_boards
      do nn = 1,5
        if ((.not.any(masks(:,nn,n))).or.(.not.any(masks(nn,:,n)))) then
            ans = sum(boards(:,:,n),masks(:,:,n))*draw
            return
        end if
      end do
    end do
  end do 
  return

2 won = .true. 
  do
    next_c = prev_c + index(input(1)%p(prev_c+1:),",")
    if (next_c .eq. prev_c) exit
    read(input(1)%p(prev_c:next_c),*) draw
    prev_c = next_c + 1

    where (boards == draw)
      masks = .false.
    end where

    do n = 1,n_boards 
      if ( .not. won(n) ) cycle
      do nn = 1,5
        if ((.not.any(masks(:,nn,n))).or.(.not.any(masks(nn,:,n)))) then
          won(n) = .false.
          if (.not.any(won)) then
            ans = sum(boards(:,:,n),masks(:,:,n))*draw
            return
          end if
        end if
      end do
    end do
  end do 
  return
  
end subroutine

