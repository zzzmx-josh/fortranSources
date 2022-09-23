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
    julian_to_date, julian_to_date_and_week_and_day, ndays, print_date, year_and_day_to_date, less_than


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

  logical function less_than(x1, x2)
    implicit none
    type (date), intent (in) :: x1
    type (date), intent (in) :: x2

    if (calendar_to_julian(x1)<calendar_to_julian(x2)) then
      less_than = .true.
    else
      less_than = .false.
    end if
  end function

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
