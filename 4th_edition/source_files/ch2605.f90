module running_average_module
  implicit none

contains
  function running_average(r, how_many) result (rarray)
    integer, intent (in) :: how_many
    real, intent (in), allocatable, dimension (:) :: r
    real, allocatable, dimension (:) :: rarray
    integer :: i
    real :: sum = 0.0

    allocate (rarray(1:how_many))
    do i = 1, how_many
      sum = sum + r(i)
      rarray(i) = sum/i
    end do
  end function
end module
module read_data_module
  implicit none

contains
  subroutine read_data(file_name, raw_data, how_many)
    implicit none
    character (len=*), intent (in) :: file_name
    integer, intent (in) :: how_many
    real, intent (out), allocatable, dimension (:) :: raw_data
    integer :: i

    allocate (raw_data(1:how_many))
    open (unit=1, file=file_name, status='old')
    do i = 1, how_many
      read (unit=1, fmt=*) raw_data(i)
    end do

  end subroutine

end module

program ch2605
  use running_average_module
  use read_data_module
  implicit none
  integer :: how_many
  character (len=20) :: file_name
  real, allocatable, dimension (:) :: raw_data
  real, allocatable, dimension (:) :: ra
  integer :: i

  print *, ' how many data items are there?'
  read *, how_many
  print *, ' what is the file name?'
  read '(a)', file_name
  call read_data(file_name, raw_data, how_many)
  allocate (ra(1:how_many))
  ra = running_average(raw_data, how_many)
  do i = 1, how_many
    print *, raw_data(i), ' ', ra(i)
  end do
end program
