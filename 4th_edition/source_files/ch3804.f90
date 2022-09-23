include 'ch3804_date_module.f90'
include 'ch3804_generic_sort_module.f90'
include 'timing_module.f90'

program ch3804

  use date_module
  use generic_sort_module
  use timing_module

  implicit none
  integer :: i
  integer, parameter :: n = 1000000
  integer, dimension (1:n) :: julian_dates
  type (date), dimension (n) :: x
  character *20 :: heading

  call start_timing()
  print *, ' '

  open (unit=100, file='julian_dates.dat', form='unformatted')

  heading = 'open'
  print 100, heading, time_difference()
100 format (a20, f7.3)

  read (100) julian_dates
  heading = 'read'
  print 100, heading, time_difference()

  do i = 1, n
    x(i) = julian_to_date(julian_dates(i))
  end do

  heading = 'copy'
  print 100, heading, time_difference()

  call sort(x, n)

  heading = 'sort'
  print 100, heading, time_difference()
  print *, ' '


  do i = 1, n, 100000
    print *, print_date(x(i))
  end do

  print *, ' '
  call end_timing()

end program

