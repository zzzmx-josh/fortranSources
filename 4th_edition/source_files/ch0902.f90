program ch0902
  implicit none
  integer :: big = 10
  integer :: i

  do i = 1, 40
    print 100, i, big
    big = big*10
  end do
100 format (' ', i3, '  ', i12)
end program
