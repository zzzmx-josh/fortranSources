program ch1204
  implicit none
  real, dimension (5) :: x = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)
! elemental function
  print *, ' sine of ', x, ' = ', sin(x)
! transformational function
  print *, ' sum of ', x, ' = ', sum(x)
end program
