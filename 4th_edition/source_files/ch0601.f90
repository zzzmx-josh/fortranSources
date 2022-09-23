program ch0601
  implicit none
  real :: total = 0.0, average = 0.0
  real, dimension (1:12) :: rainfall
  integer :: month

  print *, ' type in the rainfall values'
  print *, ' one per line'
  do month = 1, 12
    read *, rainfall(month)
  end do
  do month = 1, 12
    total = total + rainfall(month)
  end do
  average = total/12
  print *, ' Average monthly rainfall was'
  print *, average
end program
