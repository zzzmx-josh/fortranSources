program ch0803
  implicit none
  integer, dimension (-5:5) :: x
  integer :: i

  x(-5:-1) = -1
  x(0) = 0
  x(1:5) = 1
  do i = -5, 5
    print *, ' ', i, ' ', x(i)
  end do
end program
