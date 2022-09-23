program ch0810
  implicit none
  real :: total = 0.0, average = 0.0
  real, dimension (12) :: rainfall = (/ 3.1, 2.0, 2.4, 2.1, 2.2, 2.2, 1.8, 2.2, 2.7, 2.9, 3.1, 3.1 /)

  total = sum(rainfall)
  average = total/12
  print *, ' Average monthly rainfall was'
  print *, average
end program
