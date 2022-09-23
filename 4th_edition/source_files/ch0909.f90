program ch0909
  implicit none
  integer :: i

  do i = 1, 4
    print 100, i, i*i
    print 110, i, i*i
    print 120, i, i*i
100 format (' ', i2, '  ', i4)
110 format (' ', i2, '  ', i4)
120 format (1x, i2, 2x, i4)
  end do
end program
