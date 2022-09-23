program ch0919
  implicit none
  integer :: t
  character *30 :: fmt_100 = '('' '', i3, '' * 12 = '', i3)'

  print *, ' '
  print *, ' Twelve times table'
  print *, ' '
  do t = 1, 12
    write (unit=*, fmt=fmt_100) t, t*12
  end do
end program
