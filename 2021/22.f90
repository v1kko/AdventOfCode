subroutine solver22(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  type boxx
    logical :: toggle
    integer :: x1, x2, y1, y2, z1, z2
  end type
  type(char_p)   :: input(:)
  integer(int64) :: ans
  integer        :: part, n, idx(2), n_boxes
  type(boxx)     :: boxes(size(input))
  logical        :: init_area(-50:50,-50:50,-50:50)
  init_area = .false.

  do n=1,size(input)
    idx(1) = index(input(n)%p," ") -1
    if (input(n)%p(:idx(1)) == "on") then 
      boxes(n)%toggle = .true.
    else
      boxes(n)%toggle = .false.
    endif
    idx(1) =  index(input(n)%p,"=")+1
    idx(2) =  index(input(n)%p,".")-1
    read(input(n)%p(idx(1):idx(2)),*) boxes(n)%x1
    idx(1) =  idx(2)+3
    idx(2) =  index(input(n)%p(idx(1):),",") + idx(1) -2
    read(input(n)%p(idx(1):idx(2)),*) boxes(n)%x2
    idx(1) = idx(2)+4
    idx(2) =  index(input(n)%p(idx(1):),".") + idx(1) -2
    read(input(n)%p(idx(1):idx(2)),*) boxes(n)%y1
    idx(1) = idx(2) + 3
    idx(2) =  index(input(n)%p(idx(1):),",") + idx(1) -2
    read(input(n)%p(idx(1):idx(2)),*) boxes(n)%y2
    idx(1) = idx(2) + 4
    idx(2) =  index(input(n)%p(idx(1):),".") + idx(1) -2
    read(input(n)%p(idx(1):idx(2)),*) boxes(n)%z1
    idx(1) = idx(2) + 3
    read(input(n)%p(idx(1):),*) boxes(n)%z2
  end do

  ans = 0

  if (part == 1) then
    n_boxes = 0
    do n = 1, size(input)
      boxes(n)%x1 = max(boxes(n)%x1,-50)
      boxes(n)%x2 = min(boxes(n)%x2, 50)
      boxes(n)%y1 = max(boxes(n)%y1,-50)
      boxes(n)%y2 = min(boxes(n)%y2, 50)
      boxes(n)%z1 = max(boxes(n)%z1,-50)
      boxes(n)%z2 = min(boxes(n)%z2, 50)
      if (has_area(boxes(n))) then
        n_boxes = n_boxes + 1
        boxes(n_boxes) = boxes(n)
      end if
    end do
  else
    n_boxes = size(boxes)
  end if

  do n = 1, n_boxes
    if (boxes(n)%toggle) ans = ans + visible(boxes(n),boxes(n+1:size(boxes)))
  end do

contains

pure recursive function visible(box, boxes) result(nvis)
  implicit none
  integer(int64)   :: nvis
  type(boxx), intent(in) :: box, boxes(:)
  type(boxx) :: newbox
  integer           :: n, xn, yn, zn
  nvis = 0
    
  do n = 1,size(boxes)  
    if (intersect(box,boxes(n))) then
      do xn = 1,3
        do yn = 1,3
          do zn = 1,3
            if ((xn==2).and.(yn==2).and.(zn==2)) cycle
            if (xn == 1) then
              newbox%x1 = box%x1 
              newbox%x2 = boxes(n)%x1 - 1 
            else if ( xn == 2) then
              newbox%x1 = max(boxes(n)%x1,box%x1)
              newbox%x2 = min(boxes(n)%x2,box%x2) 
            else
              newbox%x1 = boxes(n)%x2 + 1
              newbox%x2 = box%x2  
            end if

            if (yn == 1) then
              newbox%y1 = box%y1 
              newbox%y2 = boxes(n)%y1 - 1 
            else if ( yn == 2) then
              newbox%y1 = max(boxes(n)%y1,box%y1)
              newbox%y2 = min(boxes(n)%y2,box%y2) 
            else
              newbox%y1 = boxes(n)%y2 + 1
              newbox%y2 = box%y2  
            end if

            if (zn == 1) then
              newbox%z1 = box%z1 
              newbox%z2 = boxes(n)%z1 - 1 
            else if ( zn == 2) then
              newbox%z1 = max(boxes(n)%z1,box%z1)
              newbox%z2 = min(boxes(n)%z2,box%z2) 
            else
              newbox%z1 = boxes(n)%z2 + 1
              newbox%z2 = box%z2  
            end if

            if (has_area(newbox)) nvis = nvis + visible(newbox, boxes(n+1:)) 
          end do
        end do 
      end do 
      return
    end if
  end do
  nvis = area(box)

end function

elemental function intersect(a,b)
  implicit none
  type(boxx), intent(in) :: a, b
  logical :: intersect

  if ( (b%x1 <= a%x2) .and. (b%x2 >= a%x1) &
 .and. (b%y1 <= a%y2) .and. (b%y2 >= a%y1) &
 .and. (b%z1 <= a%z2) .and. (b%z2 >= a%z1)) then
    intersect = .true.
  else
    intersect = .false.
  end if
end function

elemental function has_area(box)
  implicit none
  type(boxx), intent(in) :: box
  logical :: has_area
  if ( (box%x1 > box%x2) &
  .or. (box%y1 > box%y2) &
  .or. (box%z1 > box%z2)) then
    has_area = .false.
  else 
    has_area = .true.
  end if
end function

elemental function area(box)
  implicit none
  type(boxx), intent(in) :: box
  integer(int64) :: area
  area = int(box%x2+1-box%x1,int64) &
       * int(box%y2+1-box%y1,int64) &
       * int(box%z2+1-box%z1,int64)
end function

end subroutine
