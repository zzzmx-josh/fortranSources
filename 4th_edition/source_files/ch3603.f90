program ch3603

  use ieee_arithmetic

  implicit none
  integer :: i
  real :: x = 1.0
  logical :: overflow_happened = .false.

  if (ieee_support_datatype(x)) then
    print *, ' IEEE support for default precision'
  end if

  do i = 1, 50
    if (overflow_happened) then
      print *, ' overflow occurred '
      print *, ' program terminates'
      stop 20
    else
      print 100, i, x
100   format (' ', i3, ' ', e12.4)
    end if
    x = x*10.0
    call ieee_get_flag(ieee_overflow, overflow_happened)
  end do
end program
