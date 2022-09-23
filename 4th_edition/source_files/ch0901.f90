program ch0901
  implicit none
  integer :: t

  print *, ' '
  print *, ' Twelve times table'
  print *, ' '
  do t = 1, 12
    print 100, t, t*12
  end do
100 format (' ', i3, ' * 12 = ', i3)
end program
