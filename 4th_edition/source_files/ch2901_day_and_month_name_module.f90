module day_and_month_name_module

  implicit none

  character (9) :: day(0:6) = (/ 'Sunday   ', 'Monday   ', 'Tuesday  ', 'Wednesday', 'Thursday ', 'Friday   ', 'Saturday ' /)
  character (9) :: month(1:12) = (/ 'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', 'July     ', &
    'August   ', 'September', 'October  ', 'November ', 'December ' /)

end module
