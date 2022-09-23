program ch3205
  use mpi
  implicit none
  integer :: error_number
  integer :: this_process_number
  integer :: number_of_processes
  integer, dimension (mpi_status_size) :: status
  integer, allocatable, dimension (:) :: x
  integer :: n
  integer, parameter :: factor = 5
  integer :: i, j, k
  integer :: start
  integer :: end
  integer :: recv_start

  call mpi_init(error_number)
  call mpi_comm_size(mpi_comm_world, number_of_processes, error_number)
  call mpi_comm_rank(mpi_comm_world, this_process_number, error_number)
  n = number_of_processes*factor
  allocate (x(1:n))
  x = 0
  start = (factor*this_process_number) + 1
  end = factor*(this_process_number+1)
  print 100, this_process_number, start, end
  do i = start, end
    x(i) = i*factor
  end do
  do i = 1, n
    print 110, this_process_number, i, x(i)
  end do
  if (this_process_number==0) then
    do i = 1, number_of_processes - 1
      recv_start = (factor*i) + 1
      call mpi_recv(x(recv_start), factor, mpi_integer, i, 1, mpi_comm_world, status, error_number)
    end do
  else
    call mpi_send(x(start), factor, mpi_integer, 0, 1, mpi_comm_world, error_number)
  end if
  if (this_process_number==0) then
    do i = 1, n
      print 120, i, factor, x(i)
    end do
  end if
  call mpi_finalize(error_number)
100 format (' Process number = ', i3, ' start ', i3, ' end ', i3)
110 format (1x, i4, ' i ', i4, ' x(i) ', i4)
120 format (1x, i4, ' * ', i2, ' = ', i5)
end program
