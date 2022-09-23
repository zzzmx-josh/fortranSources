program ch0802
! This program reads in a grid of temperatures
! (degrees Fahrenheit) at 25 grid references
! and converts them to degrees Celsius
  implicit none
  integer, parameter :: n = 5
  real, dimension (1:n, 1:n) :: fahrenheit, celsius
  integer :: long, lat
!
! read in the temperatures
!
  do lat = 1, n
    print *, ' For Latitude= ', lat
    do long = 1, n
      print *, ' For Longitude', long
      read *, fahrenheit(lat, long)
    end do
  end do
!
! Conversion applied to all values
!
  celsius = 5.0/9.0*(fahrenheit-32.0)
  print *, celsius
  print *, fahrenheit
end program
