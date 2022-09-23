program ch1103
  implicit none
  character (len=20) :: station, file_name
  integer :: i, io_stat_number, filestat, flen, uno
  integer, parameter :: nmonths = 12
  integer, dimension (1:nmonths) :: year, month
  real, dimension (1:nmonths) :: rainfall, sunshine
  real :: rain_sum
  real :: rain_average
  real :: sun_sum
  real :: sun_average

  do
    print *, 'input weather station'
    print *, ' or "end" to stop program'
    read '(a)', station
    if (station=='end') exit
    flen = len_trim(station)
    file_name = station(1:flen) // 'data.txt'
    open (newunit=uno, file=file_name, iostat=filestat, status='old')
    if (filestat/=0) then
      print *, 'error opening file ', file_name
      print *, 'Retype the file name'
      cycle
    end if
    do i = 1, 7
      read (unit=uno, fmt='(a)')
    end do
    do i = 1, nmonths
      read (unit=uno, fmt=100, iostat=io_stat_number) year(i), month(i), rainfall(i), sunshine(i)
100   format (3x, i4, 2x, i2, 27x, f4.1, 3x, f5.1)
      if (io_stat_number/=0) then
        print *, ' error reading record ', i + 8, ' so following results incorrect:'
        exit
      end if
    end do
    close (unit=uno)
    rain_sum = sum(rainfall)/25.4
    sun_sum = sum(sunshine)
    rain_average = rain_sum/nmonths
    sun_average = sun_sum/nmonths
    write (unit=*, fmt=110) station
110 format (/, /, 'Station = ', a, /)
    write (unit=*, fmt=120) year(1), month(1)
120 format (2x, 'Start ', i4, 2x, i2)
    write (unit=*, fmt=130) year(12), month(12)
130 format (2x, 'End   ', i4, 2x, i2)
    write (unit=*, fmt=140)
140 format (19x, ' Yearly Monthly', /, 19x, ' Sum Average')
    write (unit=*, fmt=150) rain_sum, rain_average
150 format ('Rainfall (inches) ', f7.2, 2x, f7.2)
    write (unit=*, fmt=160) sun_sum, sun_average
160 format ('Sunshine ', f7.2, 2x, f7.2)
  end do
end program
