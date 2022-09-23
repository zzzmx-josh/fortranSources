include 'precision_module.f90'
include 'statistics_module.f90'
include 'timing_module.f90'

program ch2502

  use precision_module
  use statistics_module
  use timing_module

  implicit none
  integer :: n
  real (sp), allocatable, dimension (:) :: x
  real (sp) :: x_m, x_sd, x_median
  real (dp), allocatable, dimension (:) :: y
  real (dp) :: y_m, y_sd, y_median
  real (qp), allocatable, dimension (:) :: z
  real (qp) :: z_m, z_sd, z_median
  character *20, dimension (3) :: heading = [ '  Allocate    ', '  Random      ', '  Statistics  ' ]

  call start_timing()
  n = 50000000
  print *, ' n = ', n

  print *, ' Single precision'

  allocate (x(1:n))
  print 100, heading(1), time_difference()
100 format (a20, 2x, f8.3)
  call random_number(x)
  print 100, heading(2), time_difference()
  call calculate_statistics(x, n, x_m, x_sd, x_median)
  print 100, heading(3), time_difference()
  write (unit=*, fmt=110) x_m
110 format (' Mean               = ', f10.6)
  write (unit=*, fmt=120) x_sd
120 format (' Standard deviation = ', f10.6)
  write (unit=*, fmt=130) x_median
130 format (' Median             = ', f10.6)
  deallocate (x)

  print *, ' Double precision'

  allocate (y(1:n))
  print 100, heading(1), time_difference()
  call random_number(y)
  print 100, heading(2), time_difference()
  call calculate_statistics(y, n, y_m, y_sd, y_median)
  print 100, heading(3), time_difference()
  write (unit=*, fmt=110) y_m
  write (unit=*, fmt=120) y_sd
  write (unit=*, fmt=130) y_median
  deallocate (y)

  print *, ' Quad precision'

  allocate (z(1:n))
  print 100, heading(1), time_difference()
  call random_number(z)
  print 100, heading(2), time_difference()
  call calculate_statistics(z, n, z_m, z_sd, z_median)
  print 100, heading(3), time_difference()
  write (unit=*, fmt=110) z_m
  write (unit=*, fmt=120) z_sd
  write (unit=*, fmt=130) z_median
  deallocate (z)

end program
