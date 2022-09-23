program ch3605

  use ieee_arithmetic
  implicit none

  integer :: i
  real :: computed_sum
  real :: real_sum
  integer :: array_size

  logical :: inexact_happened = .false.
  integer :: allocate_status

  character *13, dimension (3) :: heading = (/ '   10,000,000', '  100,000,000', '1,000,000,000' /)

  real, allocatable, dimension (:) :: x

  if (ieee_support_datatype(x)) then
    print *, ' IEEE support for default precision'
  end if

! 10,000,000

  array_size = 10000000

  do i = 1, 3
    write (unit=*, fmt=100) array_size, heading(i)
100 format (' Array size = ', i15, 2x, a13)
    allocate (x(1:array_size), stat=allocate_status)
    if (allocate_status/=0) then
      print *, ' Allocate fails, program ends'
      stop
    end if
    x = 1.0
    computed_sum = sum(x)
    call ieee_get_flag(ieee_inexact, inexact_happened)
    real_sum = array_size*1.0
    write (unit=*, fmt=110) computed_sum
110 format (' Computed sum = ', e12.4)
    write (unit=*, fmt=120) real_sum
120 format (' Real sum     = ', e12.4)
    if (inexact_happened) then
      print *, ' inexact arithmetic'
      print *, ' in the summation'
      print *, ' program terminates'
      stop 20
    end if
    deallocate (x)
    array_size = array_size*10
  end do

end program
