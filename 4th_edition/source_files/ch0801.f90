program ch0801
  implicit none
  integer, parameter :: n = 12
  real, dimension (1:n) :: rainfall_ins = 0.0
  real, dimension (1:n) :: rainfall_cms = 0.0
  integer :: month

  print *, ' Input the rainfall values in inches'
  read *, rainfall_ins
  rainfall_cms = rainfall_ins*2.54
  do month = 1, n
    print *, ' ', month, ' ', rainfall_ins(month), ' ', rainfall_cms(month)
  end do
end program
