include 'precision_module.f90'
include 'ch2701_link_module.f90'

program ch2701
  use precision_module
  use link_module
  implicit none
  integer, parameter :: wp = dp
  type (link(real_kind=wp)), pointer :: root, current
  integer :: i = 0
  integer :: error = 0
  integer :: io_stat_number = 0
  real (wp), allocatable, dimension (:) :: x

  allocate (root)
  print *, ' type in some numbers'
  read (unit=*, fmt=*, iostat=io_stat_number) root%n
  if (io_stat_number>0) then
    error = error + 1
  else if (io_stat_number<0) then
    nullify (root%next)
  else
    i = i + 1
    allocate (root%next)
  end if
  current => root
  do while (associated(current%next))
    current => current%next
    read (unit=*, fmt=*, iostat=io_stat_number) current%n
    if (io_stat_number>0) then
      error = error + 1
    else if (io_stat_number<0) then
      nullify (current%next)
    else
      i = i + 1
      allocate (current%next)
    end if
  end do
  print *, i, ' items read'
  print *, error, ' items in error'
  allocate (x(1:i))
  i = 1
  current => root
  do while (associated(current%next))
    x(i) = current%n
    i = i + 1
    print *, current%n
    current => current%next
  end do
  print *, x
end program
