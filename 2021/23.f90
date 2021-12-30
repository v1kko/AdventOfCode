subroutine solver23(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64)  :: ans
  integer         :: part
  integer,parameter :: dist(7,8) = reshape( &
  (/  3, 2, 2, 4, 6, 8, 9 &  
   ,  4, 3, 3, 5, 7, 9,10 &  
   ,  5, 4, 2, 2, 4, 6, 7 &  
   ,  6, 5, 3, 3, 5, 7, 8 &  
   ,  7, 6, 4, 2, 2, 4, 5 &  
   ,  8, 7, 5, 3, 3, 5, 6 &  
   ,  9, 8, 6, 4, 2, 2, 3 &  
   , 10, 9, 7, 5, 3, 3, 4 /) ,(/7,8/))
  character       :: rooms(15)
  character(len=3):: dum
  type(char_p)    :: input(:)

  read(input(3)%p,'((A3)7(A1))') dum,rooms(1) &
                               , dum,rooms(3) &
                               , dum,rooms(5) &
                               , dum,rooms(7) 
  read(input(4)%p,'((A3)7(A1))') dum,rooms(2) &
                               , dum,rooms(4) &
                               , dum,rooms(6) &
                               , dum,rooms(8) 
  rooms(9:) = '.'
  ans = solve(rooms)
  write(*,*) rooms

contains
recursive function easysolve(rooms, a, b) result(score)
  implicit none
  integer(int64) :: score
  integer :: a, b
  character(len=1), intent(in) :: rooms(15)
  character(len=1) :: newrooms(15)
  newrooms = rooms
  newrooms(a) = '.'
  newrooms(b) = rooms(a)
  score = solve(newrooms) + dist(max(a,b)-8,min(a,b))*cost(rooms(a))
end function

recursive function solve (rooms) result(score)
  implicit none
  integer(int64) :: score
  integer :: dum
  character(len=1), intent(in) :: rooms(15)
  score = huge(dum)

  if (all(rooms(1:8) == (/'A','A','B','B','C','C','D','D'/))) then
    score = 0
    return
  end if

  if (((rooms(1) /= 'A') .or. (rooms(2) /= 'A')) .and. (rooms(1) /= '.')) then
    if (rooms(10) == '.') then
      score = min(easysolve(rooms,1,10), score)
      if (rooms(9) == '.') then
        score = min(easysolve(rooms,1,9), score)
      end if
    end if
    if (rooms(11) == '.') then
      score = min(easysolve(rooms,1,11), score)
      if (rooms(12) == '.') then
        score = min(easysolve(rooms,1,12), score)
        if (rooms(13) == '.') then
          score = min(easysolve(rooms,1,13), score)
          if (rooms(14) == '.') then
            score = min(easysolve(rooms,1,14), score)
            if (rooms(15) == '.') then
              score = min(easysolve(rooms,1,15), score)
            end if
          end if
        end if
      end if
    end if
  end if
  if ((rooms(2) /= 'A') .and. (rooms(2) /= '.') .and. (rooms(1) == '.')) then
    if (rooms(10) == '.') then
      score = min(easysolve(rooms,2,10), score)
      if (rooms(9) == '.') then
        score = min(easysolve(rooms,2,9), score)
      end if
    end if
    if (rooms(11) == '.') then
      score = min(easysolve(rooms,2,11), score)
      if (rooms(12) == '.') then
        score = min(easysolve(rooms,2,12), score)
        if (rooms(13) == '.') then
          score = min(easysolve(rooms,2,13), score)
          if (rooms(14) == '.') then
            score = min(easysolve(rooms,2,14), score)
            if (rooms(15) == '.') then
              score = min(easysolve(rooms,2,15), score)
            end if
          end if
        end if
      end if
    end if
  end if

  if (((rooms(3) /= 'B') .or. (rooms(4) /= 'B')).and. (rooms(3) /= '.')) then
    if (rooms(11) == '.') then
      score = min(easysolve(rooms,3,11), score)
      if (rooms(10) == '.') then
        score = min(easysolve(rooms,3,10), score)
        if (rooms(9) == '.') then
          score = min(easysolve(rooms,3,9), score)
        end if
      end if
    end if
    if (rooms(12) == '.') then
      score = min(easysolve(rooms,3,12), score)
      if (rooms(13) == '.') then
        score = min(easysolve(rooms,3,13), score)
        if (rooms(14) == '.') then
          score = min(easysolve(rooms,3,14), score)
          if (rooms(15) == '.') then
            score = min(easysolve(rooms,3,15), score)
          end if
        end if
      end if
    end if
  end if
  if ((rooms(4) /= 'B') .and. (rooms(4) /= '.') .and. (rooms(3) == '.')) then
    if (rooms(11) == '.') then
      score = min(easysolve(rooms,4,11), score)
      if (rooms(10) == '.') then
        score = min(easysolve(rooms,4,10), score)
        if (rooms(9) == '.') then
          score = min(easysolve(rooms,4,9), score)
        end if
      end if
    end if
    if (rooms(12) == '.') then
      score = min(easysolve(rooms,4,12), score)
      if (rooms(13) == '.') then
        score = min(easysolve(rooms,4,13), score)
        if (rooms(14) == '.') then
          score = min(easysolve(rooms,4,14), score)
          if (rooms(15) == '.') then
            score = min(easysolve(rooms,4,15), score)
          end if
        end if
      end if
    end if
  end if

  if (((rooms(5) /= 'C') .or. (rooms(6) /= 'C')) .and. (rooms(5) /= '.')) then
    if (rooms(12) == '.') then
      score = min(easysolve(rooms,5,12), score)
      if (rooms(11) == '.') then
        score = min(easysolve(rooms,5,11), score)
        if (rooms(10) == '.') then
          score = min(easysolve(rooms,5,10), score)
          if (rooms(9) == '.') then
            score = min(easysolve(rooms,5,9), score)
          end if
        end if
      end if
    end if
    if (rooms(13) == '.') then
      score = min(easysolve(rooms,5,13), score)
      if (rooms(14) == '.') then
        score = min(easysolve(rooms,5,14), score)
        if (rooms(15) == '.') then
          score = min(easysolve(rooms,5,15), score)
        end if
      end if
    end if
  end if
  if ((rooms(6) /= 'C') .and. (rooms(6) /= '.') .and. (rooms(5) == '.')) then
    if (rooms(12) == '.') then
      score = min(easysolve(rooms,6,12), score)
      if (rooms(11) == '.') then
        score = min(easysolve(rooms,6,11), score)
        if (rooms(10) == '.') then
          score = min(easysolve(rooms,6,10), score)
          if (rooms(9) == '.') then
            score = min(easysolve(rooms,6,9), score)
          end if
        end if
      end if
    end if
    if (rooms(13) == '.') then
      score = min(easysolve(rooms,6,13), score)
      if (rooms(14) == '.') then
        score = min(easysolve(rooms,6,14), score)
        if (rooms(15) == '.') then
          score = min(easysolve(rooms,6,15), score)
        end if
      end if
    end if
  end if

  if (((rooms(7) /= 'D')  .or. (rooms(8) /= 'D')) .and. (rooms(7) /= '.')) then
    if (rooms(13) == '.') then
      score = min(easysolve(rooms,7,13), score)
      if (rooms(12) == '.') then
        score = min(easysolve(rooms,7,12), score)
        if (rooms(11) == '.') then
          score = min(easysolve(rooms,7,11), score)
          if (rooms(10) == '.') then
            score = min(easysolve(rooms,7,10), score)
            if (rooms(9) == '.') then
              score = min(easysolve(rooms,7,9), score)
            end if
          end if
        end if
      end if
    end if
    if (rooms(14) == '.') then
      score = min(easysolve(rooms,7,14), score)
      if (rooms(15) == '.') then
        score = min(easysolve(rooms,7,15), score)
      end if
    end if
  end if
  if ((rooms(8) /= 'D') .and. (rooms(8) /= '.') .and. (rooms(7) == '.')) then
    if (rooms(13) == '.') then
      score = min(easysolve(rooms,8,13), score)
      if (rooms(12) == '.') then
        score = min(easysolve(rooms,8,12), score)
        if (rooms(11) == '.') then
          score = min(easysolve(rooms,8,11), score)
          if (rooms(10) == '.') then
            score = min(easysolve(rooms,8,10), score)
            if (rooms(9) == '.') then
              score = min(easysolve(rooms,8,9), score)
            end if
          end if
        end if
      end if
    end if
    if (rooms(14) == '.') then
      score = min(easysolve(rooms,8,14), score)
      if (rooms(15) == '.') then
        score = min(easysolve(rooms,8,15), score)
      end if
    end if
  end if

  if (rooms(10) == '.') then
    if (rooms(9) == 'A') then
      if (rooms(1) == '.') then
        if (rooms(2) == 'A') then
          score = min(easysolve(rooms,9,1), score)
        end if
        if (rooms(2) == '.') then
          score = min(easysolve(rooms,9,2), score)
        end if
      end if
    end if
    if (rooms(11) == '.') then
      if (rooms(9) == 'B') then
        if (rooms(3) == '.') then
          if (rooms(4) == 'B') then
            score = min(easysolve(rooms,9,3), score)
          end if
          if (rooms(4) == '.') then
            score = min(easysolve(rooms,9,4), score)
          end if
        end if
      end if
      if (rooms(12) == '.') then
        if (rooms(9) == 'C') then
          if (rooms(5) == '.') then
            if (rooms(6) == 'C') then
              score = min(easysolve(rooms,9,5), score)
            end if
            if (rooms(6) == '.') then
              score = min(easysolve(rooms,9,6), score)
            end if
          end if
        end if
        if (rooms(13) == '.') then
          if (rooms(9) == 'D') then
            if (rooms(7) == '.') then
              if (rooms(8) == 'D') then
                score = min(easysolve(rooms,9,7), score)
              end if
              if (rooms(8) == '.') then
                score = min(easysolve(rooms,9,8), score)
              end if
            end if
          end if
        end if
      end if
    end if
  end if

  if (rooms(10) == 'A') then
    if (rooms(1) == '.') then
      if (rooms(2) == 'A') then
        score = min(easysolve(rooms,10,1), score)
      end if
      if (rooms(2) == '.') then
        score = min(easysolve(rooms,10,2), score)
      end if
    end if
  end if
  if (rooms(11) == '.') then
    if (rooms(10) == 'B') then
      if (rooms(3) == '.') then
        if (rooms(4) == 'B') then
          score = min(easysolve(rooms,10,3), score)
        end if
        if (rooms(4) == '.') then
          score = min(easysolve(rooms,10,4), score)
        end if
      end if
    end if
    if (rooms(12) == '.') then
      if (rooms(10) == 'C') then
        if (rooms(5) == '.') then
          if (rooms(6) == 'C') then
            score = min(easysolve(rooms,10,5), score)
          end if
          if (rooms(6) == '.') then
            score = min(easysolve(rooms,10,6), score)
          end if
        end if
      end if
      if (rooms(13) == '.') then
        if (rooms(10) == 'D') then
          if (rooms(7) == '.') then
            if (rooms(8) == 'D') then
              score = min(easysolve(rooms,10,7), score)
            end if
            if (rooms(8) == '.') then
              score = min(easysolve(rooms,10,8), score)
            end if
          end if
        end if
      end if
    end if
  end if

  if (rooms(11) == 'A') then
    if (rooms(1) == '.') then
      if (rooms(2) == 'A') then
        score = min(easysolve(rooms,11,1), score)
      end if
      if (rooms(2) == '.') then
        score = min(easysolve(rooms,11,2), score)
      end if
    end if
  end if
  if (rooms(11) == 'B') then
    if (rooms(3) == '.') then
      if (rooms(4) == 'B') then
        score = min(easysolve(rooms,11,3), score)
      end if
      if (rooms(4) == '.') then
        score = min(easysolve(rooms,11,4), score)
      end if
    end if
  end if
  if (rooms(12) == '.') then
    if (rooms(11) == 'C') then
      if (rooms(5) == '.') then
        if (rooms(6) == 'C') then
          score = min(easysolve(rooms,11,5), score)
        end if
        if (rooms(6) == '.') then
          score = min(easysolve(rooms,11,6), score)
        end if
      end if
    end if
    if (rooms(13) == '.') then
      if (rooms(11) == 'D') then
        if (rooms(7) == '.') then
          if (rooms(8) == 'D') then
            score = min(easysolve(rooms,11,7), score)
          end if
          if (rooms(8) == '.') then
            score = min(easysolve(rooms,11,8), score)
          end if
        end if
      end if
    end if
  end if

  if (rooms(11) == '.') then
    if (rooms(12) == 'A') then
      if (rooms(1) == '.') then
        if (rooms(2) == 'A') then
          score = min(easysolve(rooms,12,1), score)
        end if
        if (rooms(2) == '.') then
          score = min(easysolve(rooms,12,2), score)
        end if
      end if
    end if
  end if
  if (rooms(12) == 'B') then
    if (rooms(3) == '.') then
      if (rooms(4) == 'B') then
        score = min(easysolve(rooms,12,3), score)
      end if
      if (rooms(4) == '.') then
        score = min(easysolve(rooms,12,4), score)
      end if
    end if
  end if
  if (rooms(12) == 'C') then
    if (rooms(5) == '.') then
      if (rooms(6) == 'C') then
        score = min(easysolve(rooms,12,5), score)
      end if
      if (rooms(6) == '.') then
        score = min(easysolve(rooms,12,6), score)
      end if
    end if
  end if
  if (rooms(13) == '.') then
    if (rooms(12) == 'D') then
      if (rooms(7) == '.') then
        if (rooms(8) == 'D') then
          score = min(easysolve(rooms,12,7), score)
        end if
        if (rooms(8) == '.') then
          score = min(easysolve(rooms,12,8), score)
        end if
      end if
    end if
  end if

  if (rooms(12) == '.') then
    if (rooms(13) == 'B') then
      if (rooms(3) == '.') then
        if (rooms(4) == 'B') then
          score = min(easysolve(rooms,13,3), score)
        end if
        if (rooms(4) == '.') then
          score = min(easysolve(rooms,13,4), score)
        end if
      end if
    end if
    if (rooms(11) == '.') then
      if (rooms(13) == 'A') then
        if (rooms(1) == '.') then
          if (rooms(2) == 'A') then
            score = min(easysolve(rooms,13,1), score)
          end if
          if (rooms(2) == '.') then
            score = min(easysolve(rooms,13,2), score)
          end if
        end if
      end if
    end if
  end if
  if (rooms(13) == 'C') then
    if (rooms(5) == '.') then
      if (rooms(6) == 'C') then
        score = min(easysolve(rooms,13,5), score)
      end if
      if (rooms(6) == '.') then
        score = min(easysolve(rooms,13,6), score)
      end if
    end if
  end if
  if (rooms(13) == 'D') then
    if (rooms(7) == '.') then
      if (rooms(8) == 'D') then
        score = min(easysolve(rooms,13,7), score)
      end if
      if (rooms(8) == '.') then
        score = min(easysolve(rooms,13,8), score)
      end if
    end if
  end if

  if (rooms(13) == '.') then
    if (rooms(14) == 'C') then
      if (rooms(5) == '.') then
        if (rooms(6) == 'C') then
          score = min(easysolve(rooms,14,5), score)
        end if
        if (rooms(6) == '.') then
          score = min(easysolve(rooms,14,6), score)
        end if
      end if
    end if
    if (rooms(12) == '.') then
      if (rooms(14) == 'B') then
        if (rooms(3) == '.') then
          if (rooms(4) == 'B') then
            score = min(easysolve(rooms,14,3), score)
          end if
          if (rooms(4) == '.') then
            score = min(easysolve(rooms,14,4), score)
          end if
        end if
      end if
      if (rooms(11) == '.') then
        if (rooms(14) == 'A') then
          if (rooms(1) == '.') then
            if (rooms(2) == 'A') then
              score = min(easysolve(rooms,14,1), score)
            end if
            if (rooms(2) == '.') then
              score = min(easysolve(rooms,14,2), score)
            end if
          end if
        end if
      end if
    end if
  end if
  if (rooms(14) == 'D') then
    if (rooms(7) == '.') then
      if (rooms(8) == 'D') then
        score = min(easysolve(rooms,14,7), score)
      end if
      if (rooms(8) == '.') then
        score = min(easysolve(rooms,14,8), score)
      end if
    end if
  end if

  if (rooms(14) == '.') then
    if (rooms(15) == 'D') then
      if (rooms(7) == '.') then
        if (rooms(8) == 'D') then
          score = min(easysolve(rooms,15,7), score)
        end if
        if (rooms(8) == '.') then
          score = min(easysolve(rooms,15,8), score)
        end if
      end if
    end if
    if (rooms(13) == '.') then
      if (rooms(15) == 'C') then
        if (rooms(5) == '.') then
          if (rooms(6) == 'C') then
            score = min(easysolve(rooms,15,5), score)
          end if
          if (rooms(6) == '.') then
            score = min(easysolve(rooms,15,6), score)
          end if
        end if
      end if
      if (rooms(12) == '.') then
        if (rooms(15) == 'B') then
          if (rooms(3) == '.') then
            if (rooms(4) == 'B') then
              score = min(easysolve(rooms,15,3), score)
            end if
            if (rooms(4) == '.') then
              score = min(easysolve(rooms,15,4), score)
            end if
          end if
        end if
        if (rooms(11) == '.') then
          if (rooms(15) == 'A') then
            if (rooms(1) == '.') then
              if (rooms(2) == 'A') then
                score = min(easysolve(rooms,15,1), score)
              end if
              if (rooms(2) == '.') then
                score = min(easysolve(rooms,15,2), score)
              end if
            end if
          end if
        end if
      end if
    end if
  end if
end function

pure function cost(letter)
  implicit none
  integer :: cost
  character, intent(in) :: letter

  if (letter == 'A') cost = 1
  if (letter == 'B') cost = 10
  if (letter == 'C') cost = 100
  if (letter == 'D') cost = 1000

end function
end subroutine
