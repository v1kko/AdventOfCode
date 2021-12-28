module solvers
  type char_p
    character(len=:), allocatable :: p
  end type
  interface
    subroutine solver_interface(part,input, answer)
      use iso_fortran_env, only: int64
      import :: char_p
      integer :: part
      type(char_p) :: input(:)
      integer(int64) :: answer
    end subroutine

    subroutine qsort(array,elem_count,elem_size,compare) bind(C,name="qsort")
      use iso_c_binding
      import
      type(c_ptr),value       :: array
      integer(c_size_t),value :: elem_count
      integer(c_size_t),value :: elem_size
      type(c_funptr),value    :: compare !int(*compare)(const void *, const void *)
    end subroutine qsort !standard C library qsort
  end interface
  type solver_interface_p
    procedure(solver_interface), pointer, nopass :: p => null()
  end type
  type (solver_interface_p) :: solver_list(28)
contains
include "1.f90"
include "2.f90"
include "3.f90"
include "4.f90"
include "5.f90"
include "6.f90"
include "7.f90"
include "8.f90"
include "9.f90"
include "10.f90"
include "11.f90"
include "12.f90"
include "13.f90"
include "14.f90"
include "15.f90"
include "16.f90"
include "17.f90"
include "18.f90"
include "20.f90"
include "21.f90"
include "22.f90"
include "25.f90"

subroutine init_solvers()
implicit none
  solver_list(1)%p => solver1
  solver_list(2)%p => solver2
  solver_list(3)%p => solver3
  solver_list(4)%p => solver4
  solver_list(5)%p => solver5
  solver_list(6)%p => solver6
  solver_list(7)%p => solver7
  solver_list(8)%p => solver8
  solver_list(9)%p => solver9
  solver_list(10)%p => solver10
  solver_list(11)%p => solver11
  solver_list(12)%p => solver12
  solver_list(13)%p => solver13
  solver_list(14)%p => solver14
  solver_list(15)%p => solver15
  solver_list(16)%p => solver16
  solver_list(17)%p => solver17
  solver_list(18)%p => solver18
  solver_list(20)%p => solver20
  solver_list(21)%p => solver21
  solver_list(22)%p => solver22
  solver_list(25)%p => solver25
end subroutine
end module

module input_reader
use solvers, only :  char_p
contains
  subroutine reader(input,day)
    type(char_p), allocatable, intent(out) :: input(:)
    integer :: day
    integer :: nlines, fd, size_read
    logical :: found
    character(len=80) :: filename, buffer
    nlines = 0

    write(filename,'(I2A)') day, ".txt"
    filename = adjustl(filename)
    inquire(file=filename,exist=found)
    if (.not. found ) call abort()
    open(newunit=fd, file=filename)
    do
      read(fd, *, end=10)
      nlines = nlines + 1
    enddo
10  rewind(fd)
    allocate(input(nlines))
    do nn = 1,nlines
      input(nn)%p = ''
      do
        read(fd,'(A)',advance='no', size= size_read, iostat=iostat) buffer
        if (is_iostat_eor(iostat)) then
          input(nn)%p = input(nn)%p // buffer(:size_read)
          exit
        else
          input(nn)%p = input(nn)%p // buffer
        endif
      enddo
    enddo
    close(fd)
  end subroutine
end module

program main
use solvers, only : solver_list, init_solvers, char_p
use input_reader, only : reader
use iso_fortran_env, only: int64
implicit none
integer :: n_args, n, day, l, part
character(len=3) :: arg
integer(int64) :: answer
type(char_p), allocatable :: input(:)

call init_solvers()

n_args =  command_argument_count()

if (n_args == 0) then
  write(*,*) "Execute for single solutions with <program> [5b, ...]"
  write(*,*) "Executing all"
  do day = 1,size(solver_list)
    if (.not.associated(solver_list(day)%p)) cycle
    call reader(input,day)
    call solver_list(day)%p(1,input, answer)
    write(*,"(I2,A)",advance='no') day,"a"
    write(*,*) answer
    call solver_list(day)%p(2,input, answer)
    write(*,"(I2,A)",advance='no') day,"b"
    write(*,*) answer
    deallocate(input)
  end do
  return
end if
do n=1,n_args
  call get_command_argument(n, arg, l)
  read(arg(:l-1),*) day
  if (arg(l:) == "a") then
  part = 1
  else if (arg(l:) == "b") then
  part = 2
  else
  call abort()
  endif 
  
  call reader(input, day)
  call solver_list(day)%p(part,input, answer)
  write(*,*) arg, answer
  deallocate(input)
enddo



end program
