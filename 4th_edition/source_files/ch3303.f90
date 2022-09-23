program ch3303
  use omp_lib
  implicit none
  integer :: nthreads
  integer :: thread_number
  integer :: i

  nthreads = omp_get_max_threads()
  print *, ' Number of threads = ', nthreads
!$omp parallel do private(thread_number)
  do i = 1, nthreads
    thread_number = omp_get_thread_num()
    print *, ' Hello from thread ', thread_number
  end do
!$omp end parallel do
end program
