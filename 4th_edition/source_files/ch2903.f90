include 'ch2901_day_and_month_name_module.f90'
include 'ch2901_date_module.f90'
include 'ch2902_iso_date_module.f90'
include 'ch2903_date_wrapper_module.f90'

program ch2903

  use date_module
  use iso_date_module
  use date_wrapper_module
! use us_date_module_01

  implicit none
  integer :: i, ndiff
  integer, parameter :: n_dates = 2

  type (date_wrapper), dimension (1:n_dates) :: x

  x(1)%date = date(1, 1, 1970)
  x(2)%date = iso_date(1980, 1, 1)
! x(3)%date = us_date(1, 1, 1990)

  do i = 1, n_dates
    if (x(i)%date%date_to_day_in_year()==366) then
      print *, x(i)%date%get_year(), ' is a leap year'
    else
      print *, x(i)%date%get_year(), ' is not a leap year'
    end if
  end do

  ndiff = ndays(x(1)%date, x(2)%date)

  print *, ' Number of days = ', ndiff

  x(1)%date = date(1, 1, 1970)
  x(2)%date = iso_date(1980, 1, 1)
! x(3)%date = us_date(1, 1, 1990)

  do i = 1, n_dates
    print *, ' print date test'
    print *, ' Single parameter       ', x(i)%date%print_date()
    print *, ' day_names=false short_month_name=false ', x(i)%date%print_date(day_names=.false., short_month_name=.false.)
    print *, ' day_names=true  short_month_name=false ', x(i)%date%print_date(day_names=.true., short_month_name=.false.)
    print *, ' day_names=false short_month_name=true  ', x(i)%date%print_date(day_names=.false., short_month_name=.true.)
    print *, ' day_names=true  short_month_name=true  ', x(i)%date%print_date(day_names=.true., short_month_name=.true.)
    print *, ' digits=true                            ', x(i)%date%print_date(digits=.true.)
  end do

end program

