program ch1406
  implicit none
  integer :: i

  do i = 32, 62
    print *, i, char(i), i + 32, char(i+32), i + 64, char(i+64)
  end do
  i = 63
  print *, i, char(i), i + 32, char(i+32), i + 64, 'del'
end program
