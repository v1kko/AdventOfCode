subroutine solver6(part,input,ans)
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
  character(len=:), pointer :: t, d
  integer :: ts, td, ds, dd, time, distance, rx1
  ans = 1
  t => input(1)%p
  d => input(2)%p
  ts = scan(t,' ')
  ds = scan(d,' ')

  do
    ts = verify(t(ts:),' ')+ts-1
    ds = verify(d(ds:),' ')+ds-1
    dd = scan(d(ds:),' ')+ds-2
    td = scan(t(ts:),' ')
    if (td == 0) then
      td = len(t)
      dd = len(d)
    else
      td = td+ts-2
    end if
    read(t(ts:td),*) time
    read(d(ds:dd),*) distance
    rx1 = int(ceiling((time-(time**2-4*distance)**0.5)/2))
    if ((time-rx1)*rx1 == distance) rx1 = rx1 + 1

    ans = ans * ( time - rx1 - rx1 + 1) 

    if (td ==  len(t)) exit
    ts = td + 1
    ds = dd + 1
  end do
end subroutine
subroutine solver_b(input,ans)
  implicit none
  integer(int64) :: ans
  type(char_p), target :: input(:)
  character(len=:), pointer :: t, d
  integer :: ts, td, ds, dd
  integer(int64) :: time, distance, tt, dt, rx1
  t => input(1)%p
  d => input(2)%p
  ts = scan(t,' ')
  ds = scan(d,' ')
  time = 0
  distance = 0

  do
    ts = verify(t(ts:),' ')+ts-1
    ds = verify(d(ds:),' ')+ds-1
    dd = scan(d(ds:),' ')+ds-2
    td = scan(t(ts:),' ')
    if (td == 0) then
      td = len(t)
      dd = len(d)
    else
      td = td+ts-2
    end if
    read(t(ts:td),*) tt
    read(d(ds:dd),*) dt
    time = time*10**(td-ts+1)+tt
    distance = distance*10**(dd-ds+1)+dt

    if (td ==  len(t)) exit
    ts = td + 1
    ds = dd + 1
  end do

  rx1 = int(ceiling((real(time,real64)-(real(time,real64)**2-4*real(distance,real64))**0.5)/2))
  if ((time-rx1)*rx1 == distance) rx1 = rx1 + 1

  ans = time - rx1 - rx1 + 1

end subroutine
end subroutine
