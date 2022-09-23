program ch0916
  real, dimension (10, 10) :: y
  integer :: nrows = 6
  integer :: ncols = 7
  integer :: i, j
  integer :: k = 0

  do i = 1, nrows
    do j = 1, ncols
      k = k + 1
      y(i, j) = k
    end do
  end do
  write (unit=*, fmt=100) y
100 format (1x, 10f10.4)
end program
