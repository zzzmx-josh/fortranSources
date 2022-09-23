include 'ch2207_date_module.f90'

program ch2207
  use date_module, only: calendar_to_julian, &
    date, date_, date_iso, date_us, &
    date_to_day_in_year, date_to_weekday_number, &
    get_day, get_month, get_year, &
    julian_to_date_and_week_and_day, ndays, &
    print_date, year_and_day_to_date

  implicit none
  integer :: i
  integer, parameter :: n = 3
  type (date), dimension (1:n) :: x

  x(1) = date_(11, 2, 1952)
  x(2) = date_us(2, 11, 1952)
  x(3) = date_iso(1952, 2, 11)

  do i = 1, 3
    print *, print_date(x(i))
  end do

end program ch2207
