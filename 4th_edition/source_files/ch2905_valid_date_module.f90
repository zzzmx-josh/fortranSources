module valid_date_module

  implicit none

contains

  logical function leap_year(year)
    implicit none
    integer, intent (in) :: year

    if ((year/4)*4==year) then
      leap_year = .true.
      if ((year/400)*400==year) then
        leap_year = .true.
      else if ((year/100)*100==year) then
        leap_year = .false.
      end if
    else
      leap_year = .false.
    end if
  end function

  subroutine check_date(day, month, year, ifail)
    implicit none

    integer, intent (in) :: day
    integer, intent (in) :: month
    integer, intent (in) :: year
    integer, intent (inout) :: ifail

    integer, parameter :: n_months = 12
    integer, dimension (1:n_months) :: days_in_month = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

!   Initialise ifail to 0

    ifail = 0

!   Simple test for Gregorian start date
!   This is a warning. See the book for more
!   details
!   about dates and calendars.

    if (year<1582) then
      ifail = 1
    end if

    if ((month<1) .or. (month>12)) then
      ifail = ifail + 2
      return
    end if

!   Now have a valid month

!   reset in case of leap year in previous call

    days_in_month(2) = 28

    if (leap_year(year)) then
      days_in_month(2) = 29
    end if

    if ((day<1) .or. (day>days_in_month(month))) then
      ifail = ifail + 4
      return
    end if

    return

  end subroutine

end module
