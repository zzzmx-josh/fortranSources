module date_module_interface

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
    procedure, nopass      :: julian_to_date
    procedure, nopass      :: julian_to_date_and_week_and_day
    procedure, nopass      :: ndays
    procedure, pass (this) :: print_date
    procedure, pass (this) :: set_day
    procedure, pass (this) :: set_month
    procedure, pass (this) :: set_year
    procedure, nopass      :: year_and_day_to_date

  end type date

  interface date
    module procedure date_constructor
  end interface date

  interface
    module function calendar_to_julian(this) result (ival)
      implicit none
      integer :: ival
      class (date), intent (in) :: this
    end function calendar_to_julian
  end interface

  interface
    type (date) module function date_constructor(dd, mm, yyyy)

      implicit none
      integer, intent (in) :: dd, mm, yyyy
    end function date_constructor
  end interface

  interface
    integer module function date_to_day_in_year(this)
      implicit none
      class (date), intent (in) :: this
      intrinsic modulo
    end function date_to_day_in_year
  end interface

  interface
    integer module function date_to_weekday_number(this)
      implicit none
      class (date), intent (in) :: this
      intrinsic modulo
    end function date_to_weekday_number
  end interface

  interface
    module function get_day(this)
      implicit none
      integer :: get_day
      class (date), intent (in) :: this
    end function get_day
  end interface

  interface
    module function get_month(this)
      implicit none
      integer :: get_month
      class (date), intent (in) :: this
    end function get_month
  end interface

  interface
    module function get_year(this)
      implicit none
      integer :: get_year
      class (date), intent (in) :: this
    end function get_year
  end interface

  interface
    module function julian_to_date(julian)
      implicit none
      type (date) :: julian_to_date
      integer, intent (in) :: julian
    end function julian_to_date
  end interface

  interface
    module subroutine julian_to_date_and_week_and_day (jd, d, wd, ddd)
      implicit none
      integer, intent (in) :: jd
      type (date), intent (out) :: d
      integer, intent (out) :: wd, ddd
    end subroutine julian_to_date_and_week_and_day
  end interface

  interface
    module function ndays(date1, date2)
      implicit none
      integer :: ndays
      class (date), intent (in) :: date1, date2
    end function ndays
  end interface

  interface
    module function print_date(this, day_names, short_month_name, digits)
      implicit none
      class (date), intent (in) :: this
      logical, optional, intent (in) :: day_names, short_month_name, digits
      character (len=40) :: print_date
    end function print_date
  end interface

  interface
    module subroutine set_day(this, d)
      implicit none
      integer, intent (in) :: d
      class (date), intent (inout) :: this
    end subroutine set_day
  end interface

  interface
    module subroutine set_month(this, m)
      implicit none
      integer, intent (in) :: m
      class (date), intent (inout) :: this
    end subroutine set_month
  end interface

  interface
    module subroutine set_year(this, y)
      implicit none
      integer, intent (in) :: y
      class (date), intent (inout) :: this
    end subroutine set_year
  end interface

  interface
    module function year_and_day_to_date(year, day_in_year)
      use day_and_month_name_module
      implicit none
      type (date) :: year_and_day_to_date
      integer, intent (in) :: day_in_year, year
    end function year_and_day_to_date
  end interface

  public :: calendar_to_julian, &
            date_to_day_in_year, &
            date_to_weekday_number, get_day, &
            get_month, &
            get_year, julian_to_date, &
            julian_to_date_and_week_and_day, &
            ndays, print_date, &
            set_day, set_month, set_year, &
            year_and_day_to_date

end module date_module_interface
