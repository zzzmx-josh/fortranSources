include 'precision_module.f90'

program ch2607

  use precision_module, wp => sp

  implicit none
  integer :: i
  integer, parameter :: n = 4
  real (wp), dimension (n) :: x1 = [ 1.1_wp, 1.01_wp, 1.001_wp, 1.0001_wp ]
  real (wp), dimension (n) :: x2 = [ 1.2_wp, 1.02_wp, 1.002_wp, 1.0002_wp ]
  real (wp), dimension (n) :: x3 = [ 0.1_wp, 0.01_wp, 0.001_wp, 0.0001_wp ]
  real (wp), dimension (n) :: rel_error = 0.0_wp
  real (wp), dimension (n) :: abs_error = 0.0_wp
  real (wp) :: z

  character (len=11), dimension (n) :: heading_1 = [ '1 in 10    ', '1 in 100   ', '1 in 1,000 ', '1 in 10,000' ]
  character (len=6), dimension (n) :: heading_2 = [ '1.1   ', '1.01  ', '1.001 ', '1.0001' ]
  character (len=15), dimension (2) :: heading_3 = [ 'Absolute error ', 'Relative error ' ]

  do i = 1, n
    z = x2(i) - x1(i)
    abs_error(i) = abs(z-x3(i))
    rel_error(i) = abs_error(i)/x3(i)
    print *, ' ', heading_1(i), '             ', heading_2(i)
    print *, ' Calculated =         ', z, '   ', heading_3(1), abs_error(i)
    print *, ' Expected   =         ', x3(i), '   ', heading_3(2), rel_error(i)
  end do

end program
