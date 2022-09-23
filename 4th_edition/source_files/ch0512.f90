program ch0512
  implicit none
  real :: x1 = 1.0
  real :: x2 = 0.1
  integer i

  print *, ' x1 = ', x1
  print *, ' x2 = ', x2
  do i = 1, 990
    x1 = x1 + x2
  end do
  print *, ' x1 = ', x1
end program
