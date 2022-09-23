module date_module
  implicit none

  private

  type, public :: date
    private
    integer :: day
    integer :: month
    integer :: year
    integer :: date_type = 1
  end type

  character (9) :: day(0:6) = (/ 'Sunday   ', 'Monday   ', 'Tuesday  ', 'Wednesday', 'Thursday ', 'Friday   ', 'Saturday ' /)
  character (9) :: month(1:12) = (/ 'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', 'July     ', &
    'August   ', 'September', 'October  ', 'November ', 'December ' /)

  public :: calendar_to_julian, date_, date_iso, date_us, date_to_day_in_year, date_to_weekday_number, get_day, get_month, &
    get_year, julian_to_date, julian_to_date_and_week_and_day, ndays, print_date, year_and_day_to_date

contains

  function date_(dd, mm, yyyy) result (x)
    implicit none
    type (date) :: x
    integer, intent (in) :: dd, mm, yyyy
    integer :: dt = 1

    x = date(dd, mm, yyyy, dt)
  end function

  function date_iso(yyyy, mm, dd) result (x)
    implicit none
    type (date) :: x
    integer, intent (in) :: dd, mm, yyyy
    integer :: dt = 3

    x = date(dd, mm, yyyy, dt)
  end function

  function date_us(mm, dd, yyyy) result (x)
    implicit none
    type (date) :: x
    integer, intent (in) :: dd, mm, yyyy
    integer :: dt = 2

    x = date(dd, mm, yyyy, dt)
  end function

  include 'date_module_include_code.f90'

  function print_date(x, day_names, short_month_name, digits)
    implicit none
    type (date), intent (in) :: x
    logical, optional, intent (in) :: day_names, short_month_name, digits
    character (30) :: print_date
    integer :: pos
    logical :: want_day, want_short_month_name, want_digits
    integer :: l, t
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
!   Start of code dependent on date_type
!   day month year
    if (x%date_type==1) then
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
    else if (x%date_type==2) then
!     month day year
      if (want_digits) then
        write (print_date(1:2), '(i2)') x%month
        print_date(3:3) = '/'
        write (print_date(4:5), '(i2)') x%day
        print_date(6:6) = '/'
        write (print_date(7:10), '(i4)') x%year
      else
        pos = 1
        if (want_short_month_name) then
          print_date(pos:pos+2) = month(x%month)(1:3)
          pos = pos + 4
        else
          print_date(pos:) = month(x%month)
          pos = len_trim(print_date) + 2
        end if
        if (want_day) then
          t = date_to_weekday_number(x)
          l = len_trim(day(t))
          print_date(pos:pos+l) = trim(day(t)) // ' '
          pos = len_trim(print_date) + 2
        end if
        write (print_date(pos:pos+1), '(i2)') x%day
        pos = pos + 3
        write (print_date(pos:pos+3), '(i4)') x%year
      end if
    else if (x%date_type==3) then
!     year month day
      if (want_digits) then
        write (print_date(1:4), '(i4)') x%year
        print_date(5:5) = '/'
        write (print_date(6:7), '(i2)') x%month
        print_date(8:8) = '/'
        write (print_date(9:10), '(i2)') x%day
      else
        pos = 1
        write (print_date(pos:pos+3), '(i4)') x%year
        pos = pos + 5
        if (want_short_month_name) then
          print_date(pos:pos+2) = month(x%month)(1:3)
          pos = pos + 4
        else
          print_date(pos:) = month(x%month)
          pos = len_trim(print_date) + 2
        end if
        if (want_day) then
          t = date_to_weekday_number(x)
          l = len_trim(day(t))
          print_date(pos:pos+l) = trim(day(t))
          pos = pos + l + 1
        end if
        write (print_date(pos:pos+1), '(i2)') x%day
      end if
    end if
    return
  end function

end module
