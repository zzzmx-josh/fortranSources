program ch0706
  implicit none
  real, dimension (-20:20) :: current
  real :: resistance
  integer :: voltage

  print *, ' type in the resistance'
  read *, resistance
  do voltage = -20, 20
    current(voltage) = voltage/resistance
    print *, voltage, ' ', current(voltage)
  end do
end program
