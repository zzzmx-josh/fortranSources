program ch0506
  implicit none
  real :: z = 0.0
  real :: b = 1.0e30
  real :: c = 1.0e30
  real :: d = 1.0e30

  z = b*c/d
  print *, z
  z = b*(c/d)
  print *, z
end program
