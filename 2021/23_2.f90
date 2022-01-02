subroutine solver23(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer(int64)  :: ans
  integer         :: part, n, rlen, ncand, ndone, scores(1000000), idx, score
  character(len=23) :: start, final, cur
  character(len=23) :: candidates(1000000)
  character(len=23) :: done(1000000)
  character(len=3)  :: dum
  type(char_p)      :: input(:)

  ndone = 0

  if (part == 1) then
    rlen = 2
  else
    rlen = 4
  end if

  read(input(3)%p,'((A3)7(A1))') dum,start(1:1) &
                               , dum,start(rlen*1+1:rlen*1+1) &
                               , dum,start(rlen*2+1:rlen*2+1) &
                               , dum,start(rlen*3+1:rlen*3+1) 
  read(input(4)%p,'((A3)7(A1))') dum,start(rlen*1:rlen*1) &
                               , dum,start(rlen*2:rlen*2) &
                               , dum,start(rlen*3:rlen*3) &
                               , dum,start(rlen*4:rlen*4) 
  if (part == 2) then
    start(2:2) = 'D'
    start(3:3) = 'D'
    start(6:6) = 'C'
    start(7:7) = 'B'
    start(10:10) = 'B'
    start(11:11) = 'A'
    start(14:14) = 'A'
    start(15:15) = 'C'
  end if
  start(rlen*4+1:) = '.......'
  final(rlen*4+1:) = '.......'
  do n = 1,rlen
    final(n:n) = 'A'
    final(rlen+n:rlen+n) = 'B'
    final(rlen*2+n:rlen*2+n) = 'C'
    final(rlen*3+n:rlen*3+n) = 'D'
  end do

  candidates(1) = start
  scores(1) = 0
  ncand = 1

  do while (cur /= final)
    idx = minloc(scores(:ncand),1,back=.true.)
    cur = candidates(idx)
    score = scores(idx)
    candidates(idx) = candidates(ncand)
    scores(idx) = scores(ncand)
    ncand = ncand - 1
    ndone = ndone + 1
    done(ndone) = cur
    call get_candidates(cur, score)
  end do

  write(*,*) start, final, score
  ans = score


contains
subroutine get_candidates(room, score)
  implicit none
  character(len=23) :: room
  integer :: score, score2, n, m, o, nn
  character(len=1), parameter :: letter(0:3) = (/'A','B','C','D'/)

  do n = 0, 3
    nn = n
    do m = rlen,1,-1
      if ((room(rlen*n+m:rlen*n+m) /= letter(n) ) .and. &
          (room(rlen*n+m:rlen*n+m) /= '.' )) then
        do o = 1,m
          if (room(rlen*n+o:rlen*n+o) /= '.')  then
            !9, 10
            if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n < 1))  then
              if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n < 2))  then
                if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n < 3))  then
                  if ((room(rlen*4+1:rlen*4+1) == '.') .and. (room(rlen*4+2:rlen*4+2) == '.')) then
                    score2 = (o-1+n*2+3)*cost(room(rlen*n+o:rlen*n+o))
                    call easyadd(room,rlen*n+o, rlen*4+1, score2 + score)
                  end if
                  if ((room(rlen*4+2:rlen*4+2) == '.')) then
                    score2 = (o-1+n*2+2)*cost(room(rlen*n+o:rlen*n+o))
                    call easyadd(room,rlen*n+o, rlen*4+2, score2 + score)
                  end if
                end if
              end if
            end if

            !11
            if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n < 2))  then
              if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n < 3))  then
                if ((room(rlen*4+3:rlen*4+3) == '.'))  then
                  nn = n - 1
                  if (n == 0) nn = 0
                  score2 = (o-1+nn*2+2)*cost(room(rlen*n+o:rlen*n+o))
                  call easyadd(room,rlen*n+o, rlen*4+3, score2 + score)
                end if
              end if
            end if

            !12
            if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n > 0))  then
              if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n < 3))  then
                if ((room(rlen*4+4:rlen*4+4) == '.'))  then
                  nn = n - 2
                  if (n == 0) nn = 1
                  if (n == 1) nn = 0
                  score2 = (o-1+nn*2+2)*cost(room(rlen*n+o:rlen*n+o))
                  call easyadd(room,rlen*n+o, rlen*4+4, score2 + score)
                end if
              end if
            end if

            !13
            if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n > 0))  then
              if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n > 1))  then
                if ((room(rlen*4+5:rlen*4+5) == '.'))  then
                  if (n == 0) nn = 2
                  if (n == 1) nn = 1
                  if (n == 2) nn = 0
                  if (n == 3) nn = 0
                  score2 = (o-1+nn*2+2)*cost(room(rlen*n+o:rlen*n+o))
                  call easyadd(room,rlen*n+o, rlen*4+5, score2 + score)
                end if
              end if
            end if

            !14, 15
            if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n > 0))  then
              if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n > 1))  then
                if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n > 2))  then
                nn = 3-n
                  if ((room(rlen*4+6:rlen*4+6) == '.') .and. (room(rlen*4+7:rlen*4+7) == '.')) then
                    score2 = (o-1+nn*2+3)*cost(room(rlen*n+o:rlen*n+o))
                    call easyadd(room,rlen*n+o, rlen*4+7, score2 + score)
                  end if
                  if ((room(rlen*4+6:rlen*4+6) == '.')) then
                    score2 = (o-1+nn*2+2)*cost(room(rlen*n+o:rlen*n+o))
                    call easyadd(room,rlen*n+o, rlen*4+6, score2 + score)
                  end if
                end if
              end if
            end if
            exit
          end if
        end do
        exit
      end if

      ! place back
      if (room(rlen*n+m:rlen*n+m) == '.' ) then
        if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n < 1))  then
          if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n < 2))  then
            if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n < 3))  then
              if ((room(rlen*4+1:rlen*4+1) == letter(n)) .and. (room(rlen*4+2:rlen*4+2) == '.')) then
                score2 = (m+n*2+2)*cost(room(rlen*4+1:rlen*4+1))
                call easyadd(room,rlen*4+1,rlen*n+m, score2 + score)
              end if
              if ((room(rlen*4+2:rlen*4+2) == letter(n))) then
                score2 = (m+n*2+1)*cost(room(rlen*4+2:rlen*4+2))
                call easyadd(room,rlen*4+2,rlen*n+m, score2 + score)
              end if
            end if
          end if
        end if

        !11
        if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n < 2))  then
          if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n < 3))  then
            if ((room(rlen*4+3:rlen*4+3) == letter(n)))  then
              nn = n - 1
              if (n == 0) nn = 0
              score2 = (m+nn*2+1)*cost(room(rlen*4+3:rlen*4+3))
              call easyadd(room,rlen*4+3, rlen*n+m, score2 + score)
            end if
          end if
        end if

        !12
        if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n > 0))  then
          if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n < 3))  then
            if ((room(rlen*4+4:rlen*4+4) == letter(n)))  then
              nn = n - 2
              if (n == 0) nn = 1
              if (n == 1) nn = 0
              score2 = (m+nn*2+1)*cost(room(rlen*4+4:rlen*4+4))
              call easyadd(room,rlen*4+4, rlen*n+m, score2 + score)
            end if
          end if
        end if

        !13
        if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n > 0))  then
          if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n > 1))  then
            if ((room(rlen*4+5:rlen*4+5) == letter(n)))  then
              if (n == 0) nn = 2
              if (n == 1) nn = 1
              if (n == 2) nn = 0
              if (n == 3) nn = 0
              score2 = (m+nn*2+1)*cost(room(rlen*4+5:rlen*4+5))
              call easyadd(room,rlen*4+5, rlen*n+m, score2 + score)
            end if
          end if
        end if

        !14, 15
        if ((room(rlen*4+3:rlen*4+3) == '.') .or. (n > 0))  then
          if ((room(rlen*4+4:rlen*4+4) == '.') .or. (n > 1))  then
            if ((room(rlen*4+5:rlen*4+5) == '.') .or. (n > 2))  then
            nn = 3-n
              if ((room(rlen*4+6:rlen*4+6) == '.') .and.  (room(rlen*4+7:rlen*4+7) == letter(n))) then
                score2 = (m+nn*2+2)*cost(room(rlen*4+7:rlen*4+7))
                call easyadd(room,rlen*4+7, rlen*n+m, score2 + score)
              end if
              if ((room(rlen*4+6:rlen*4+6) == letter(n))) then
                score2 = (m+nn*2+1)*cost(room(rlen*4+6:rlen*4+6))
                call easyadd(room,rlen*4+6, rlen*n+m, score2 + score)
              end if
            end if
          end if
        end if

        exit
      end if
    end do
  end do
end subroutine

subroutine easyadd(room, a, b, score)
  implicit none
  integer :: score
  integer :: a, b, idx
  character(len=23), intent(in) :: room
  character(len=23) :: newroom
  newroom = room
  newroom(a:a) = '.'
  newroom(b:b) = room(a:a)

  if (findloc(done(:ndone), newroom,1) /= 0) return
  idx = findloc(candidates(:ncand), newroom,1)
  if (idx == 0) then
    ncand = ncand + 1
    candidates(ncand) = newroom
    scores(ncand) = score
  else
    if (scores(idx) > score) scores(idx) = score
  end if
end subroutine

function cost(letter)
  implicit none
  integer :: cost
  character, intent(in) :: letter

  if (letter == 'A') then 
    cost = 1
  else if (letter == 'B') then
    cost = 10
  else if (letter == 'C') then 
    cost = 100
  else if (letter == 'D') then
    cost = 1000
  else
    call abort()
  end if
end function
end subroutine
