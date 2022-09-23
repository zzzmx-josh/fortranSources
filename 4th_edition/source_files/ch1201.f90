program ch1201
  implicit none
  real :: x

  print *, ' type in an angle (in radians)'
  read *, x
  print *, ' Sine of ', x, ' = ', sin(x)
end program
