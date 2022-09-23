program ch3301
  use omp_lib
  implicit none
  integer :: nthreads
  integer :: thread_number
  integer :: i

  nthreads = omp_get_max_threads()
  print *, ' Number of threads = ', nthreads
!$omp parallel do
  do i = 1, nthreads
    print *, ' Hello from thread ', omp_get_thread_num()
  end do
!$omp end parallel do
end program
