program ch1206
  implicit none
  integer :: year, metcyc, century, error1, error2, day
  integer :: epact, luna, temp
! a program to calculate the date of easter
  print *, ' input the year for which easter'
  print *, ' is to be calculated'
  print *, ' enter the whole year, e.g. 1978 '
  read *, year
! calculating the year in the 19 year
! metonic cycle using variable metcyc
  metcyc = mod(year, 19) + 1
  if (year<=1582) then
    day = (5*year)/4
    epact = mod(11*metcyc-4, 30) + 1
  else
!   calculating the century-century
    century = (year/100) + 1
!   accounting for arithmetic inaccuracies
!   ignores leap years etc.
    error1 = (3*century/4) - 12
    error2 = ((8*century+5)/25) - 5
!   locating Sunday
    day = (5*year/4) - error1 - 10
!   locating the epact(full moon)
    temp = 11*metcyc + 20 + error2 - error1
    epact = mod(temp, 30)
    if (epact<=0) then
      epact = 30 + epact
    end if
    if ((epact==25 .and. metcyc>11) .or. epact==24) then
      epact = epact + 1
    end if
  end if
! finding the full moon
  luna = 44 - epact
  if (luna<21) then
    luna = luna + 30
  end if
! locating easter Sunday
  luna = luna + 7 - (mod(day+luna,7))
! locating the correct month
  if (luna>31) then
    luna = luna - 31
    print *, ' for the year ', year
    print *, ' easter falls on April ', luna
  else
    print *, ' for the year ', year
    print *, ' easter falls on march ', luna
  end if
end program
