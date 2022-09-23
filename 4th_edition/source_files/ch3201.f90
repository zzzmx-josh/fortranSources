program ch3201
  use mpi
  implicit none
  integer :: error_number
  integer :: this_process_number
  integer :: number_of_processes

  call mpi_init(error_number)
  call mpi_comm_size(mpi_comm_world, number_of_processes, error_number)
  call mpi_comm_rank(mpi_comm_world, this_process_number, error_number)
  print *, ' Hello from process ', this_process_number, ' of ', number_of_processes, 'processes!'
  call mpi_finalize(error_number)
end program
