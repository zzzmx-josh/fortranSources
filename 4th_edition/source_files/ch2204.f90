module ragged_module
  type ragged
    real, allocatable, dimension (:) :: rainfall
  end type
end module

program ch2204
  use ragged_module
  implicit none
  integer :: i
  integer :: nr
  integer, allocatable, dimension (:) :: nc
  type (ragged), allocatable, dimension (:) :: station

  print *, ' enter number of stations'
  read *, nr
  allocate (station(1:nr))
  allocate (nc(1:nr))
  do i = 1, nr
    print *, ' enter the number of data values ', 'for station ', i
    read *, nc(i)
    allocate (station(i)%rainfall(1:nc(i)))
    if (nc(i)==0) then
      cycle
    end if
    print *, ' Type in the values for station ', i
    read *, station(i)%rainfall(1:nc(i))
  end do
  print *, '  Row    N    Data'
  do i = 1, nr
    print 100, i, nc(i), station(i)%rainfall(1:nc(i))
100 format (3x, i3, 2x, i3, 2x, 12(1x,f6.2))
  end do
end program
