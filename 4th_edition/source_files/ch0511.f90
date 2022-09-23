program ch0511
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  real, parameter :: pi = 3.1415926535897931
  real (dp), parameter :: pid = 3.1415926535897931_dp
  real :: area, r = 2.0
  real (dp) :: aread, rd = 2.0_dp

  area = pi*r*r
  aread = pid*rd*rd
  print 100, r, rd
100 format ('r     = ', f22.18, /, 'rd    = ', f22.18)
  print 110, area, aread
110 format ('area  = ', f22.18, /, 'aread = ', f22.18, /, 16x, '  ######')
end program
