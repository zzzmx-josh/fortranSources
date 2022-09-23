program ch0805
  implicit none
  integer, parameter :: n = 12
  real :: total = 0.0, average = 0.0
  real, dimension (1:n) :: rainfall = (/ 3.1, 2.0, 2.4, 2.1, 2.2, 2.2, 1.8, 2.2, 2.7, 2.9, 3.1, 3.1 /)
  integer :: month

  do month = 1, n
    total = total + rainfall(month)
  end do
  average = total/n
  print *, ' Average monthly rainfall was'
  print *, average
end program
