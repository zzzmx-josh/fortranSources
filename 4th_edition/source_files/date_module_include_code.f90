function calendar_to_julian(x) result (ival)
  implicit none
  integer :: ival
  type (date), intent (in) :: x

  ival = x%day - 32075 + 1461*(x%year+4800+(x% &
    month-14)/12)/4 + 367*(x%month-2-((x%month- &
    14)/12)*12)/12 - 3*((x%year+4900+(x%month-14 &
    )/12)/100)/4
end function calendar_to_julian

function date_to_day_in_year(x)
  implicit none
  integer :: date_to_day_in_year
  type (date), intent (in) :: x
  intrinsic modulo

  date_to_day_in_year = 3055*(x%month+2)/100 - &
    (x%month+10)/13*2 - 91 + (1-(modulo(x%year, &
    4)+3)/4+(modulo(x%year,100)+99)/100-(modulo( &
    x%year,400)+399)/400)*(x%month+10)/13 + x% &
    day
end function date_to_day_in_year

function date_to_weekday_number(x)
  implicit none
  integer :: date_to_weekday_number
  type (date), intent (in) :: x
  intrinsic modulo

  date_to_weekday_number = modulo((13*(x%month+ &
    10-(x%month+10)/13*12)-1)/5+x%day+77+5*(x% &
    year+(x%month-14)/12-(x%year+ &
    (x%month-14)/12)/100*100)/4+(x%year+( &
    x%month-14)/12)/400-(x%year+(x%month- &
    14)/12)/100*2, 7)
end function date_to_weekday_number

function get_day(x)
  implicit none
  integer :: get_day
  type (date), intent (in) :: x

  get_day = x%day
end function get_day

function get_month(x)
  implicit none
  integer :: get_month
  type (date), intent (in) :: x

  get_month = x%month
end function get_month

function get_year(x)
  implicit none
  integer :: get_year
  type (date), intent (in) :: x

  get_year = x%year
end function get_year

function julian_to_date(julian) result (x)
  implicit none
  integer, intent (in) :: julian
  integer :: l, n
  type (date) :: x

  l = julian + 68569
  n = 4*l/146097
  l = l - (146097*n+3)/4
  x%year = 4000*(l+1)/1461001
  l = l - 1461*x%year/4 + 31
  x%month = 80*l/2447
  x%day = l - 2447*x%month/80
  l = x%month/11
  x%month = x%month + 2 - 12*l
  x%year = 100*(n-49) + x%year + 1
end function julian_to_date

subroutine julian_to_date_and_week_and_day(jd, &
  x, wd, ddd)
  implicit none
  integer, intent (out) :: ddd, wd
  integer, intent (in) :: jd
  type (date), intent (out) :: x

  x = julian_to_date(jd)
  wd = date_to_weekday_number(x)
  ddd = date_to_day_in_year(x)
end subroutine julian_to_date_and_week_and_day

function ndays(date1, date2)
  implicit none
  integer :: ndays
  type (date), intent (in) :: date1, date2

  ndays = calendar_to_julian(date1) - &
    calendar_to_julian(date2)
end function ndays

function year_and_day_to_date(year, day) &
  result (x)
  implicit none
  type (date) :: x
  integer, intent (in) :: day, year
  integer :: t
  intrinsic modulo

  x%year = year
  t = 0
  if (modulo(year,4)==0) then
    t = 1
  end if
  if (modulo(year,400)/=0 .and. modulo(year,100) &
    ==0) then
    t = 0
  end if
  x%day = day
  if (day>59+t) then
    x%day = x%day + 2 - t
  end if
  x%month = ((x%day+91)*100)/3055
  x%day = (x%day+91) - (x%month*3055)/100
  x%month = x%month - 2
  if (x%month>=1 .and. x%month<=12) then
    return
  end if
  write (unit=*, fmt='(a,i11,a)') '$$year_and_day&
    &_to_date: day of the year input =', day, &
    ' is out of range.'
end function year_and_day_to_date
