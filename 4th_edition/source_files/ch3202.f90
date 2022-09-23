program ch3202
  use mpi
  implicit none
  integer :: error_number
  integer :: this_process_number
  integer :: number_of_processes
  integer :: i
  integer, dimension (mpi_status_size) :: status

  call mpi_init(error_number)
  call mpi_comm_size(mpi_comm_world, number_of_processes, error_number)
  call mpi_comm_rank(mpi_comm_world, this_process_number, error_number)
  if (this_process_number==0) then
    print *, ' Hello from process ', this_process_number, ' of ', number_of_processes, 'processes.'
    do i = 1, number_of_processes - 1
      call mpi_recv(this_process_number, 1, mpi_integer, i, 1, mpi_comm_world, status, error_number)
      print *, ' Hello from process ', this_process_number, ' of ', number_of_processes, 'processes.'
    end do
  else
    call mpi_send(this_process_number, 1, mpi_integer, 0, 1, mpi_comm_world, error_number)
  end if
  call mpi_finalize(error_number)
end program
