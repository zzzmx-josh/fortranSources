program ch1302
  implicit none
  integer :: year, n, month, day, t

! calculates day and month from year and
! day-within-year
! t is an offset to account for leap years.
! Note that the first criteria is division by 4
! but that centuries are only
! leap years if divisible by 400
! not 100 (4 * 25) alone.

  print *, ' year, followed by day within year'
  read *, year, n
! checking for leap years
  if ((year/4)*4==year) then
    t = 1
    if ((year/400)*400==year) then
      t = 1
    else if ((year/100)*100==year) then
      t = 0
    end if
  else
    t = 0
  end if
! accounting for February
  if (n>(59+t)) then
    day = n + 2 - t
  else
    day = n
  end if
  month = (day+91)*100/3055
  day = (day+91) - (month*3055)/100
  month = month - 2
  print *, ' calendar date is ', day, month, year
end program
