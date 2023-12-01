subroutine solver1(part,input,ans)
  implicit none
  integer(int64) :: ans
  integer :: part, n, m, i
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  i = 0
  ans = 0

  if (part == 2) goto 2

  do n =1,size(input)
    p => input(n)%p
    do m =1,len(p)
      if (p(m:m) < ':') then
        read(p(m:m),*) i
        ans = ans + i * 10
       exit
      end if
    end do
    do m =len(p),1,-1
      if (p(m:m) < ':') then
        read(p(m:m),*) i
        ans = ans + i
       exit
      end if
    end do
  end do

  return

2 continue

  do n =1,size(input)
    p => input(n)%p
    do m =1,len(p)
      if (p(m:m) < ':') then
        read(p(m:m),*) i
        ans = ans + i * 10
       exit
      end if
      i = word_number(p(m:len(p)))
      if (i /= 0) then
        ans = ans + i * 10
        exit
      endif
    end do
    do m =len(p),1,-1
      if (p(m:m) < ':') then
        read(p(m:m),*) i
        ans = ans + i
       exit
      end if
      i = word_number(p(m:len(p)))
      if (i /= 0) then
        ans = ans + i
        exit
      endif
    end do
  end do

contains
function word_number(text)
  implicit none
  integer word_number
  character(len=*) ::  text
  integer text_len

  word_number = 0

  text_len = len(text)
  if (text_len < 3) return
  if (text(:3) == 'one') then
    word_number = 1
    return
  end if
  if (text(:3) == 'two') then
    word_number = 2
    return
  end if
  if (text(:3) == 'six') then
    word_number = 6
    return
  end if
  if (text_len < 4) return
  if (text(:4) == 'four') then
    word_number = 4
    return
  end if
  if (text(:4) == 'five') then
    word_number = 5
    return
  end if
  if (text(:4) == 'nine') then
    word_number = 9
    return
  end if
  if (text_len < 5) return
  if (text(:5) == 'three') then
    word_number = 3
    return
  end if
  if (text(:5) == 'seven') then
    word_number = 7
    return
  end if
  if (text(:5) == 'eight') then
    word_number = 8
    return
  end if
end function
end subroutine
