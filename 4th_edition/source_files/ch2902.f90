include 'ch2901_day_and_month_name_module.f90'
include 'ch2901_date_module.f90'
include 'ch2902_iso_date_module.f90'

program ch2902

  use date_module, only: calendar_to_julian, date, date_to_day_in_year, date_to_weekday_number, get_day, get_month, get_year, &
    julian_to_date, julian_to_date_and_week_and_day, ndays, print_date, year_and_day_to_date

  use iso_date_module

  implicit none
  integer :: dd, ddd, i, mm, ndiff, wd, yyyy
  integer :: julian
  integer :: val(8)
  intrinsic date_and_time
  type (iso_date) :: date1, date2, x, tx1, tx2

  call date_and_time(values=val)
  yyyy = val(1)
  mm = 10
  do i = 31, 26, -1
    x = iso_date(yyyy, mm, i)
    if (x%date_to_weekday_number()==0) then
      print *, 'Turn clocks  back to EST on: ', i, ' October ', x%get_year()
      exit
    end if
  end do
  call date_and_time(values=val)
  yyyy = val(1)
  mm = 4
  do i = 1, 8
    x = iso_date(yyyy, mm, i)
    if (x%date_to_weekday_number()==0) then
      print *, 'Turn clocks ahead to DST on: ', i, ' April   ', x%get_year()
      exit
    end if
  end do
  call date_and_time(values=val)
  yyyy = val(1)
  mm = 12
  dd = 31
  x = iso_date(yyyy, mm, dd)
  if (x%date_to_day_in_year()==366) then
    print *, x%get_year(), ' is a leap year'
  else
    print *, x%get_year(), ' is not a leap year'
  end if
  x = iso_date(1970, 1, 1)
  call julian_to_iso_date_and_week_and_day(calendar_to_julian(x), x, wd, ddd)
  if (x%get_year()/=1970 .or. x%get_month()/=1 .or. x%get_day()/=1 .or. wd/=4 .or. ddd/=1) then
    print *, 'julian_to_date_and_week_and_day failed'
    print *, ' date, wd, ddd = ', x%get_year(), x%get_month(), x%get_day(), wd, ddd
    stop
  end if
  date1 = iso_date(1984, 5, 22)
  date2 = iso_date(1983, 5, 22)
  ndiff = ndays(date1, date2)
  yyyy = 1970

  x = year_and_day_to_iso_date(yyyy, ddd)

  if (ndiff/=366) then
    print *, 'ndays failed; ndiff = ', ndiff
  else
    if (x%get_month()/=1 .and. x%get_day()/=1) then
      print *, 'year_and_day_to_date failed'
      print *, ' mma, dda = ', x%get_month(), x%get_day()
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

  tx1 = iso_date(1970, 1, 1)
  julian = tx1%calendar_to_julian()
  tx2 = julian_to_iso_date(julian)
  if (tx1%get_day()==tx2%get_day() .and. tx1%get_month()==tx2%get_month() .and. tx1%get_year()==tx2%get_year()) then
    print *, ' calendar_to_julian and '
    print *, ' julian_to_iso_date worked'
  end if

  x = iso_date(1952, 2, 11)

  print *, ' print iso date test'
  print *, ' Single parameter       ', x%print_date()
  print *, ' day_names=false short_month_name=false ', x%print_date(day_names=.false., short_month_name=.false.)
  print *, ' day_names=true  short_month_name=false ', x%print_date(day_names=.true., short_month_name=.false.)
  print *, ' day_names=false short_month_name=true  ', x%print_date(day_names=.false., short_month_name=.true.)
  print *, ' day_names=true  short_month_name=true  ', x%print_date(day_names=.true., short_month_name=.true.)
  print *, ' digits=true                            ', x%print_date(digits=.true.)

  print *, ' Test out a month'

  yyyy = 1970
  do dd = 1, 31
    x = year_and_day_to_iso_date(yyyy, dd)
    print *, x%print_date(day_names=.false., short_month_name=.true.)
  end do

end program
