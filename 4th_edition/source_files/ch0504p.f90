program ch0504p
  implicit none
  real :: light_minute, distance, elapse
  integer :: minute, second
  real :: light_year
! Light_year : Distance travelled by light
! in one year in km
! Light_minute : Distance travelled by light
! in one minute in km
! Distance : Distance from sun to earth in km
! Elapse : Time taken to travel a
! distance (Distance) in minutes
! Minute : integer number part of elapse
! Second : integer number of seconds
! equivalent to fractional part of elapse
!
  light_year = 9.46*10**12
  light_minute = light_year/(365.25*24.0*60.0)
  distance = 150.0*10**6
  elapse = distance/light_minute
  minute = elapse
  second = (elapse-minute)*60
  print *, ' Light takes ', minute, ' Minutes'
  print *, ' ', second, ' Seconds'
  print *, ' To reach the earth from sun'
end program
