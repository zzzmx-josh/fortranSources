program ch0813
  implicit none
  integer :: j1 = 1
  integer :: k1 = 2
  integer :: j2 = 1
  integer :: k2 = 5
  integer :: s1
  integer :: s2
  integer :: d1
  integer :: position

  d1 = k1 - j1 + 1
  print *, ' Row  Column    Position'
  do s1 = j1, k1
    do s2 = j2, k2
      position = 1 + (s1-j1) + (s2-j2)*d1
      print 100, s1, s2, position
100   format (3x, i2, 6x, i2, 10x, i2)
    end do
  end do

end program
