module date_module
  implicit none

  private

  type, public :: date
    private
    integer :: day
    integer :: month
    integer :: year
  end type

  character (9) :: day(0:6) = (/ 'Sunday   ', 'Monday   ', 'Tuesday  ', 'Wednesday', 'Thursday ', 'Friday   ', 'Saturday ' /)
  character (9) :: month(1:12) = (/ 'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', 'July     ', &
    'August   ', 'September', 'October  ', 'November ', 'December ' /)

  public :: calendar_to_julian, date_, date_to_day_in_year, date_to_weekday_number, get_day, get_month, get_year, &
    julian_to_date, julian_to_date_and_week_and_day, ndays, print_date, year_and_day_to_date

contains

  function calendar_to_julian(x) result (ival)
    implicit none
    integer :: ival
    type (date), intent (in) :: x

    ival = x%day - 32075 + 1461*(x%year+4800+(x%month-14)/12)/4 + 367*(x%month-2-((x%month-14)/12)*12)/12 - &
      3*((x%year+4900+(x%month-14)/12)/100)/4
  end function

  function date_(dd, mm, yyyy) result (x)
    implicit none
    type (date) :: x
    integer, intent (in) :: dd, mm, yyyy

    x = date(dd, mm, yyyy)
  end function

! functions
! "izlr"     date_to_day_in_year
! and
! "iday"     date_to_weekday_number
! are taken from remark on
! algorithm 398, by j. douglas robertson,
! cacm 15(10):918.

  function date_to_day_in_year(x)
    implicit none
    integer :: date_to_day_in_year
    type (date), intent (in) :: x
    intrinsic modulo

    date_to_day_in_year = 3055*(x%month+2)/100 - (x%month+10)/13*2 - 91 + (1-(modulo(x%year,4)+3)/4+(modulo(x%year, &
      100)+99)/100-(modulo(x%year,400)+399)/400)*(x%month+10)/13 + x%day
  end function

  function date_to_weekday_number(x)
    implicit none
    integer :: date_to_weekday_number
    type (date), intent (in) :: x
    intrinsic modulo

    date_to_weekday_number = modulo((13*(x%month+10-(x%month+10)/13*12)-1)/5+x%day+77+5*(x%year+(x%month-14)/12-(x%year+ &
      (x%month-14)/12)/100*100)/4+(x%year+(x%month-14)/12)/400-(x%year+(x%month-14)/12)/100*2, 7)
  end function

  function get_day(x)
    implicit none
    integer :: get_day
    type (date), intent (in) :: x

    get_day = x%day
  end function

  function get_month(x)
    implicit none
    integer :: get_month
    type (date), intent (in) :: x

    get_month = x%month
  end function

  function get_year(x)
    implicit none
    integer :: get_year
    type (date), intent (in) :: x

    get_year = x%year
  end function

! cdate - julian_to_date
! see cacm 1968 11(10):657,
! letter to the editor by fliegel and van
! flandern.

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
  end function

  subroutine julian_to_date_and_week_and_day(jd, x, wd, ddd)
    implicit none
    integer, intent (out) :: ddd, wd
    integer, intent (in) :: jd
    type (date), intent (out) :: x

    x = julian_to_date(jd)
    wd = date_to_weekday_number(x)
    ddd = date_to_day_in_year(x)
  end subroutine

  function ndays(date1, date2)
    implicit none
    integer :: ndays
    type (date), intent (in) :: date1, date2

    ndays = calendar_to_julian(date1) - calendar_to_julian(date2)
  end function

  function print_date(x, day_names, short_month_name, digits)
    implicit none
    type (date), intent (in) :: x
    logical, optional, intent (in) :: day_names, short_month_name, digits
    character (40) :: print_date
    integer :: pos
    logical :: want_day, want_short_month_name, want_digits
    intrinsic len_trim, present, trim

    want_day = .false.
    want_short_month_name = .false.
    want_digits = .false.
    print_date = ' '
    if (present(day_names)) then
      want_day = day_names
    end if
    if (present(short_month_name)) then
      want_short_month_name = short_month_name
    end if
    if (present(digits)) then
      want_digits = digits
    end if
    if (want_digits) then
      write (print_date(1:2), '(i2)') x%day
      print_date(3:3) = '/'
      write (print_date(4:5), '(i2)') x%month
      print_date(6:6) = '/'
      write (print_date(7:10), '(i4)') x%year
    else
      if (want_day) then
        pos = date_to_weekday_number(x)
        print_date = trim(day(pos)) // ' '
        pos = len_trim(print_date) + 2
      else
        pos = 1
        print_date = ' '
      end if
      write (print_date(pos:pos+1), '(i2)') x%day
      if (want_short_month_name) then
        print_date(pos+3:pos+5) = month(x%month)(1:3)
        pos = pos + 7
      else
        print_date(pos+3:) = month(x%month)
        pos = len_trim(print_date) + 2
      end if
      write (print_date(pos:pos+3), '(i4)') x%year
    end if

    return
  end function

! calend - year_and_day_to_date
! see acm algorithm 398,
! tableless date conversion, by
! dick stone, cacm 13(10):621.

  function year_and_day_to_date(year, day) result (x)
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
    if (modulo(year,400)/=0 .and. modulo(year,100)==0) then
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
    write (unit=*, fmt='(a,i11,a)') '$$year_and_day_to_date: day of the year input =', day, ' is out of range.'
  end function

end module

program ch2206
  use date_module, only: calendar_to_julian, date, date_, date_to_day_in_year, date_to_weekday_number, get_day, get_month, &
    get_year, julian_to_date_and_week_and_day, ndays, print_date, year_and_day_to_date

  implicit none
  integer :: dd, ddd, i, mm, ndiff, wd, yyyy
  integer :: val(8)
  intrinsic date_and_time
  type (date) :: date1, date2, x

  call date_and_time(values=val)
  yyyy = val(1)
  mm = 10
  do i = 31, 26, -1
    x = date_(i, mm, yyyy)
    if (date_to_weekday_number(x)==0) then
      print *, 'Turn clocks  back to EST on: ', i, ' October ', get_year(x)
      exit
    end if
  end do
  call date_and_time(values=val)
  yyyy = val(1)
  mm = 4
  do i = 1, 8
    x = date_(i, mm, yyyy)
    if (date_to_weekday_number(x)==0) then
      print *, 'Turn clocks ahead to DST on: ', i, ' April   ', get_year(x)
      exit
    end if
  end do
  call date_and_time(values=val)
  yyyy = val(1)
  mm = 12
  dd = 31
  x = date_(dd, mm, yyyy)
  if (date_to_day_in_year(x)==366) then
    print *, get_year(x), ' is a leap year'
  else
    print *, get_year(x), ' is not a leap year'
  end if
  x = date_(1, 1, 1970)
  call julian_to_date_and_week_and_day(calendar_to_julian(x), x, wd, ddd)
  if (get_year(x)/=1970 .or. get_month(x)/=1 .or. get_day(x)/=1 .or. wd/=4 .or. ddd/=1) then
    print *, 'julian_to_date_and_week_and_day failed'
    print *, ' date, wd, ddd = ', get_year(x), get_month(x), get_day(x), wd, ddd
    stop
  end if
  date1 = date_(22, 5, 1984)
  date2 = date_(22, 5, 1983)
  ndiff = ndays(date1, date2)
  yyyy = 1970

  x = year_and_day_to_date(yyyy, ddd)

  if (ndiff/=366) then
    print *, 'ndays failed; ndiff = ', ndiff
  else
    if (get_month(x)/=1 .and. get_day(x)/=1) then
      print *, 'year_and_day_to_date failed'
      print *, ' mma, dda = ', get_month(x), get_day(x)
    else
      print *, ' calendar_to_julian OK'
      print *, ' date_ OK'
      print *, ' date_to_day_in_year OK'
      print *, ' date_to_weekday_number OK'
      print *, ' get_day OK'
      print *, ' get_month OK'
      print *, ' get_year OK'
      print *, ' julian_to_date_and_week_and_day OK'
      print *, ' ndays OK'
      print *, ' year_and_day_to_date OK'
    end if
  end if

  x = date_(11, 2, 1952)

  print *, ' print_date test'
  print *, ' Single parameter       ', print_date(x)
  print *, ' day_names=false short_month_name=false ', print_date(x, day_names=.false., short_month_name=.false.)
  print *, ' day_names=true  short_month_name=false ', print_date(x, day_names=.true., short_month_name=.false.)
  print *, ' day_names=false short_month_name=true  ', print_date(x, day_names=.false., short_month_name=.true.)
  print *, ' day_names=true  short_month_name=true  ', print_date(x, day_names=.true., short_month_name=.true.)
  print *, ' digits=true            ', print_date(x, digits=.true.)

  print *, ' Test out a month'

  yyyy = 1970
  do dd = 1, 31
    x = year_and_day_to_date(yyyy, dd)
    print *, print_date(x, day_names=.false., short_month_name=.true.)
  end do

end program
