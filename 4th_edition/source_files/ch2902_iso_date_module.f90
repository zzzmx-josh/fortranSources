module iso_date_module

  use day_and_month_name_module

  use date_module

  implicit none

  public

  type, extends (date) :: iso_date

  contains

    procedure, pass (this) :: print_date => print_iso_date
    procedure, nopass :: julian_to_iso_date
    procedure, nopass :: julian_to_iso_date_and_week_and_day
    procedure, nopass :: year_and_day_to_iso_date

  end type

  interface iso_date
    module procedure iso_date_constructor
  end interface

contains

  type (iso_date) function iso_date_constructor(yyyy, mm, dd)

    implicit none
    integer, intent (in) :: dd, mm, yyyy

    call iso_date_constructor%set_day(dd)
    call iso_date_constructor%set_month(mm)
    call iso_date_constructor%set_year(yyyy)

  end function

  function julian_to_iso_date(julian)
    implicit none
    type (iso_date) :: julian_to_iso_date
    integer, intent (in) :: julian

    integer :: l, n

    l = julian + 68569
    n = 4*l/146097
    l = l - (146097*n+3)/4
    call julian_to_iso_date%set_year((4000*(l+1)/1461001))
    l = l - 1461*julian_to_iso_date%get_year()/4 + 31
    call julian_to_iso_date%set_month((80*l/2447))
    call julian_to_iso_date%set_day((l-2447*julian_to_iso_date%get_month()/80))
    l = julian_to_iso_date%get_month()/11
    call julian_to_iso_date%set_month((julian_to_iso_date%get_month()+2-12*l))
    call julian_to_iso_date%set_year((100*(n-49)+julian_to_iso_date%get_year()+1))

  end function

  subroutine julian_to_iso_date_and_week_and_day(jd, d, wd, ddd)
    implicit none
    integer, intent (in) :: jd
    type (iso_date), intent (out) :: d
    integer, intent (out) :: wd, ddd

    d = julian_to_iso_date(jd)
    wd = date_to_weekday_number(d)
    ddd = date_to_day_in_year(d)
  end subroutine

  function print_iso_date(this, day_names, short_month_name, digits)
    use day_and_month_name_module
    implicit none
    class (iso_date), intent (in) :: this
    logical, optional, intent (in) :: day_names, short_month_name, digits
    character (40) :: print_iso_date
    integer :: pos
    logical :: want_day, want_short_month_name, want_digits
    integer :: l, t
    intrinsic len_trim, present, trim

    want_day = .false.
    want_short_month_name = .false.
    want_digits = .false.
    print_iso_date = ' '
    if (present(day_names)) then
      want_day = day_names
    end if
    if (present(short_month_name)) then
      want_short_month_name = short_month_name
    end if
    if (present(digits)) then
      want_digits = digits
    end if
!   year month day
    if (want_digits) then
      write (print_iso_date(1:4), '(i4)') this%get_year()
      print_iso_date(5:5) = '/'
      write (print_iso_date(6:7), '(i2)') this%get_month()
      print_iso_date(8:8) = '/'
      write (print_iso_date(9:10), '(i2)') this%get_day()
    else
      pos = 1
      write (print_iso_date(pos:pos+3), '(i4)') this%get_year()
      pos = pos + 5
      if (want_short_month_name) then
        print_iso_date(pos:pos+2) = month(this%get_month())(1:3)
        pos = pos + 4
      else
        print_iso_date(pos:) = month(this%get_month())
        pos = len_trim(print_iso_date) + 2
      end if
      if (want_day) then
        t = date_to_weekday_number(this)
        l = len_trim(day(t))
        print_iso_date(pos:pos+l) = trim(day(t))
        pos = pos + l + 1
      end if
      write (print_iso_date(pos:pos+1), '(i2)') this%get_day()
    end if

  end function

  function year_and_day_to_iso_date(year, day_in_year)
    use day_and_month_name_module
    implicit none
    type (iso_date) :: year_and_day_to_iso_date
    integer, intent (in) :: day_in_year, year
    integer :: t
    intrinsic modulo

    call year_and_day_to_iso_date%set_year(year)
    t = 0
    if (modulo(year,4)==0) then
      t = 1
    end if
    if (modulo(year,400)/=0 .and. modulo(year,100)==0) then
      t = 0
    end if
    call year_and_day_to_iso_date%set_day(day_in_year)
    if (day_in_year>59+t) then
      call year_and_day_to_iso_date%set_day(year_and_day_to_iso_date%get_day()+2-t)
    end if
    call year_and_day_to_iso_date%set_month(((year_and_day_to_iso_date%get_day()+91)*100)/3055)
    call year_and_day_to_iso_date%set_day((year_and_day_to_iso_date%get_day()+91)-(year_and_day_to_iso_date%get_month( &
      )*3055)/100)
    call year_and_day_to_iso_date%set_month(year_and_day_to_iso_date%get_month()-2)
    if (year_and_day_to_iso_date%get_month()>=1 .and. year_and_day_to_iso_date%get_month()<=12) then
      return
    end if
    write (unit=*, fmt='(a,i11,a)') '$$year_and_day_to_date: day of the year input =', day_in_year, ' is out of range.'
  end function

end module

