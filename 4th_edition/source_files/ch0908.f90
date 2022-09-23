program ch0908
  implicit none
  integer :: i
  real :: r1 = 1.23456
  real :: r2 = 1.23456

  print 100
100 format (' ', '1234567890123456789012345678901')
  print 110
110 format ('  i3  g12.4         g12.4')
  do i = 1, 10
    print 120, i, r1, r2
    r1 = r1/10.0
    r2 = r2*10.0
  end do
120 format (' ', i3, '  ', g12.4, '  ', g12.4)
end program
