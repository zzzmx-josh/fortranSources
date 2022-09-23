program ch0704
! Variables used
! h1,h2,h3
! used to hold the heights above sea level
! h4
! used to hold the average of the above
! Long - used to represent the longitude
! Lat - used to represent the latitude
! both restricted to integer values.
  implicit none
  integer, parameter :: n = 3
  integer :: lat, long
  real, dimension (1:n, 1:n) :: h1, h2, h3, h4

  do lat = 1, n
    read *, (h1(lat,long), long=1, n)
  end do
  do lat = 1, n
    read *, (h2(lat,long), long=1, n)
  end do
  do lat = 1, n
    read *, (h3(lat,long), long=1, n)
  end do
  do lat = 1, n
    do long = 1, n
      h4(lat, long) = (h1(lat,long)+h2(lat,long)+h3(lat,long))/n
    end do
  end do
  do lat = 1, n
    print *, (h4(lat,long), long=1, n)
  end do
end program
