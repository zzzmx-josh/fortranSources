include 'precision_module.f90'

program ch2608

  use precision_module, wp => dp

  implicit none
  integer :: i
  integer, parameter :: n = 5

  real (wp), dimension (n) :: x1 = [ 1.000000001_wp, 1.0000000001_wp, 1.00000000001_wp, 1.000000000001_wp, 1.0000000000001_wp ]

  real (wp), dimension (n) :: x2 = [ 1.000000002_wp, 1.0000000002_wp, 1.00000000002_wp, 1.000000000002_wp, 1.0000000000002_wp ]

  real (wp), dimension (n) :: x3 = [ 0.000000001_wp, 0.0000000001_wp, 0.00000000001_wp, 0.000000000001_wp, 0.0000000000001_wp ]

  real (wp), dimension (n) :: rel_error = 0.0_wp
  real (wp), dimension (n) :: abs_error = 0.0_wp

  real (wp) :: z

  character (len=23), dimension (n) :: heading_1 = [ '1 in      1,000,000,000', '1 in     10,000,000,000', &
    '1 in    100,000,000,000', '1 in  1,000,000,000,000', '1 in 10,000,000,000,000' ]

  character *15, dimension (n) :: heading_2 = [ '1.000000001    ', '1.0000000001   ', '1.00000000001  ', '1.000000000001 ', &
    '1.0000000000001' ]

  character *15, dimension (2) :: heading_3 = [ 'Absolute error ', 'Relative error ' ]

  do i = 1, n
    z = x2(i) - x1(i)
    abs_error(i) = abs(z-x3(i))
    rel_error(i) = abs_error(i)/x3(i)
    print *, heading_1(i), ' ', heading_2(i)
    print *, ' Calculated =        ', z, ' ', heading_3(1), abs_error(i)
    print *, ' Expected   =        ', x3(i), ' ', heading_3(2), rel_error(i)
  end do

end program
