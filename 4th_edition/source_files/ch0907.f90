program ch0907
  implicit none
  integer :: i
  real :: r1 = 1.23456
  real :: r2 = 1.23456

  do i = 1, 10
    print 100, i, r1, r2
    r1 = r1/10.0
    r2 = r2*10.0
  end do
100 format (' ', i3, '   ', e12.4, '   ', e12.4)
end program
