subroutine solver2(part,input,ans)
  implicit none
  integer(int64) :: ans
  integer :: part, n, s, d, r, g, b, i
  type(char_p), target :: input(:)
  character(len=:), pointer :: p
  character(len=8) :: color
  ans = 0

  if (part == 2) goto 2

  do n =1,size(input)
    p => input(n)%p
    s = scan(p,':') + 1

    do 
      d = scan(p(s:),';,')+s-1
      if ( d == s-1) d = len(p)

      read(p(s:d),*) i, color

      select case(color)
      case('green')
        if (i > 13) goto 666
      case('red')
        if (i > 12) goto 666
      case('blue')
        if (i > 14) goto 666
      end select
      
      if (d == len(p)) exit ! End of game
      s = d + 1
    end do

    ans = ans + n
666 continue
  end do
  return

2 continue

  do n =1,size(input)
    p => input(n)%p
    s = scan(p,':') + 1
    r = 0
    g = 0
    b = 0

    do 
      d = scan(p(s:),';,')+s-1
      if ( d == s-1) d = len(p)

      read(p(s:d),*) i, color

      select case(color)
      case('green')
        g = max(i,g)
      case('red')
        r = max(i,r)
      case('blue')
        b = max(i,b)
      end select
      
      if (d == len(p)) exit ! End of game
      s = d + 1
    end do
    ans = ans + (r*g*b)
  end do

end subroutine
