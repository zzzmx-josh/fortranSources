module date_module

  use day_and_month_name_module

  implicit none

  private

  type, public :: date

    private

    integer :: day
    integer :: month
    integer :: year

  contains

    procedure, pass (this) :: calendar_to_julian
    procedure, pass (this) :: date_to_day_in_year
    procedure, pass (this) :: date_to_weekday_number
    procedure, pass (this) :: get_day
    procedure, pass (this) :: get_month
    procedure, pass (this) :: get_year
    procedure, nopass :: julian_to_date
    procedure, nopass :: julian_to_date_and_week_and_day
    procedure, nopass :: ndays
    procedure, pass (this) :: print_date
    procedure, pass (this) :: set_day
    procedure, pass (this) :: set_month
    procedure, pass (this) :: set_year
    procedure, nopass :: year_and_day_to_date

  end type

  interface date
    module procedure date_constructor
  end interface

  public :: calendar_to_julian, date_to_day_in_year, date_to_weekday_number, get_day, get_month, get_year, julian_to_date, &
    julian_to_date_and_week_and_day, ndays, print_date, set_day, set_month, set_year, year_and_day_to_date

contains

  function calendar_to_julian(this) result (ival)
    implicit none
    integer :: ival
    class (date), intent (in) :: this

    ival = this%day - 32075 + 1461*(this%year+4800+(this%month-14)/12)/4 + 367*(this%month-2-((this%month-14)/12)*12)/12 - &
      3*((this%year+4900+(this%month-14)/12)/100)/4
  end function

  type (date) function date_constructor(dd, mm, yyyy)

    implicit none
    integer, intent (in) :: dd, mm, yyyy

    date_constructor%day = dd
    date_constructor%month = mm
    date_constructor%year = yyyy

  end function

  integer function date_to_day_in_year(this)
    implicit none
    class (date), intent (in) :: this
    intrinsic modulo

    date_to_day_in_year = 3055*(this%month+2)/100 - (this%month+10)/13*2 - 91 + (1-(modulo(this%year,4)+3)/4+(modulo(this%year, &
      100)+99)/100-(modulo(this%year,400)+399)/400)*(this%month+10)/13 + this%day
  end function

  integer function date_to_weekday_number(this)
    implicit none
    class (date), intent (in) :: this
    intrinsic modulo

    date_to_weekday_number = modulo((13*(this%month+10-(this%month+10)/13*12)-1)/5+this%day+77+5*(this%year+(this%month-14)/12-( &
      this%year+(this%month-14)/12)/100*100)/4+(this%year+(this%month-14)/12)/400-(this%year+(this%month-14)/12)/100*2, 7)
  end function

  function get_day(this)
    implicit none
    integer :: get_day
    class (date), intent (in) :: this

    get_day = this%day
  end function

  function get_month(this)
    implicit none
    integer :: get_month
    class (date), intent (in) :: this

    get_month = this%month
  end function

  function get_year(this)
    implicit none
    integer :: get_year
    class (date), intent (in) :: this

    get_year = this%year
  end function

  function julian_to_date(julian)
    implicit none
    type (date) :: julian_to_date
    integer, intent (in) :: julian

    integer :: l, n

    l = julian + 68569
    n = 4*l/146097
    l = l - (146097*n+3)/4
    julian_to_date%year = (4000*(l+1)/1461001)
    l = l - 1461*julian_to_date%year/4 + 31
    julian_to_date%month = (80*l/2447)
    julian_to_date%day = (l-2447*julian_to_date%month/80)
    l = julian_to_date%month/11
    julian_to_date%month = (julian_to_date%month+2-12*l)
    julian_to_date%year = (100*(n-49)+julian_to_date%year+1)
  end function

  subroutine julian_to_date_and_week_and_day(jd, d, wd, ddd)
    implicit none
    integer, intent (in) :: jd
    type (date), intent (out) :: d
    integer, intent (out) :: wd, ddd

    d = julian_to_date(jd)
    wd = date_to_weekday_number(d)
    ddd = date_to_day_in_year(d)
  end subroutine

  function ndays(date1, date2)
    implicit none
    integer :: ndays
    class (date), intent (in) :: date1, date2

    ndays = calendar_to_julian(date1) - calendar_to_julian(date2)
  end function

  function print_date(this, day_names, short_month_name, digits)
    implicit none
    class (date), intent (in) :: this
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
      write (print_date(1:2), '(i2)') this%day
      print_date(3:3) = '/'
      write (print_date(4:5), '(i2)') this%month
      print_date(6:6) = '/'
      write (print_date(7:10), '(i4)') this%year
    else
      if (want_day) then
        pos = date_to_weekday_number(this)
        print_date = trim(day(pos)) // ' '
        pos = len_trim(print_date) + 2
      else
        pos = 1
        print_date = ' '
      end if
      write (print_date(pos:pos+1), '(i2)') this%day
      if (want_short_month_name) then
        print_date(pos+3:pos+5) = month(this%month)(1:3)
        pos = pos + 7
      else
        print_date(pos+3:) = month(this%month)
        pos = len_trim(print_date) + 2
      end if
      write (print_date(pos:pos+3), '(i4)') this%year
    end if

    return
  end function

  subroutine set_day(this, d)
    implicit none
    integer, intent (in) :: d
    class (date), intent (inout) :: this

    this%day = d
  end subroutine

  subroutine set_month(this, m)
    implicit none
    integer, intent (in) :: m
    class (date), intent (inout) :: this

    this%month = m
  end subroutine

  subroutine set_year(this, y)
    implicit none
    integer, intent (in) :: y
    class (date), intent (inout) :: this

    this%year = y
  end subroutine

  function year_and_day_to_date(year, day_in_year)
    use day_and_month_name_module
    implicit none
    type (date) :: year_and_day_to_date
    integer, intent (in) :: day_in_year, year
    integer :: t
    intrinsic modulo

    year_and_day_to_date%year = year
    t = 0
    if (modulo(year,4)==0) then
      t = 1
    end if
    if (modulo(year,400)/=0 .and. modulo(year,100)==0) then
      t = 0
    end if
    year_and_day_to_date%day = day_in_year
    if (day_in_year>59+t) then
      year_and_day_to_date%day = year_and_day_to_date%day + 2 - t
    end if
    year_and_day_to_date%month = ((year_and_day_to_date%day+91)*100)/3055
    year_and_day_to_date%day = (year_and_day_to_date%day+91) - (year_and_day_to_date%month*3055)/100
    year_and_day_to_date%month = year_and_day_to_date%month - 2
    if (year_and_day_to_date%month>=1 .and. year_and_day_to_date%month<=12) then
      return
    end if
    write (unit=*, fmt='(a,i11,a)') '$$year_and_day_to_date: day of the year input =', day_in_year, ' is out of range.'
  end function

end module
