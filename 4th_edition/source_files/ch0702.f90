program ch0702
! Variables used
! Height - used to hold the heights above sea
! level
! Long - used to represent the longitude
! Lat - used to represent the latitude
! both restricted to integer values.
! Correct - holds the correction factor
  implicit none
  integer, parameter :: n = 3
  integer :: lat, long
  real, dimension (1:n, 1:n) :: height
  real, parameter :: correct = 10.0

  do lat = 1, n
    do long = 1, n
      print *, ' type in value at ', lat, ' ', long
      read *, height(lat, long)
    end do
  end do
  do lat = 1, n
    do long = 1, n
      height(lat, long) = height(lat, long) + correct
    end do
  end do
  print *, ' Corrected data is '
  do lat = 1, n
    do long = 1, n
      print *, height(lat, long)
    end do
  end do
end program
