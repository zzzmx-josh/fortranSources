program ch1102
  implicit none
  integer :: io_stat_number = -1
  integer :: i

  do
    print *, 'input integer i:'
    read (unit=*, fmt=100, iostat=io_stat_number) i
    print *, ' iostat=', io_stat_number
    if (io_stat_number==0) exit
  end do
  print *, 'i = ', i, ' read successfully'
100 format (i3)
end program
