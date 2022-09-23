program ch1003
  implicit none
  character *20 :: file_name = 'nairndata_01.txt'
  integer, parameter :: nmonths = 12
  real, dimension (1:nmonths) :: rainfall
  real :: rain_sum
  real :: rain_average
  integer :: i

  open (unit=10, file=file_name)
  do i = 1, nmonths
    read (unit=10, fmt=100) rainfall(i)
100 format (37x, f5.1)
  end do
  close (10)
  rain_sum = sum(rainfall)/25.4
  rain_average = rain_sum/nmonths
  write (unit=*, fmt=110)
110 format (19x, 'Yearly   Monthly', /, 19x, 'Sum      Average')
  write (unit=*, fmt=120) rain_sum, rain_average
120 format ('Rainfall (inches) ', f7.2, 2x, f7.2)
end program
