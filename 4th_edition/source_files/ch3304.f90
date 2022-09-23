include 'precision_module.f90'

include 'timing_module.f90'

program ch3304
  use precision_module
  use timing_module
  use omp_lib
  implicit none
  real (dp) :: fortran_internal_pi
  real (dp) :: partial_pi
  real (dp) :: openmp_pi
  real (dp) :: width
  real (dp) :: x
  integer :: nthreads
  integer :: i
  integer :: j
  integer :: k
  integer :: n

  nthreads = omp_get_max_threads()
  fortran_internal_pi = 4.0_dp*atan(1.0_dp)
  print *, ' Maximum number of threads is ', nthreads
  do k = 1, nthreads
    call start_timing()
    n = 100000
    call omp_set_num_threads(k)
    print *, ' Number of threads = ', k
    do j = 1, 5
      width = 1.0_dp/n
      partial_pi = 0.0_dp
!$omp parallel do private(x) &
!$omp shared(width) reduction(+:partial_pi)
      do i = 1, n
        x = width*(real(i,dp)-0.5_dp)
        partial_pi = partial_pi + f(x)
      end do
!$omp end parallel do
      openmp_pi = width*partial_pi
      print 100, n, time_difference()
      print 110, openmp_pi, abs(openmp_pi-fortran_internal_pi)
      n = n*10
    end do
  end do
100 format (' N intervals = ', i12, ' time =', f8.3)
110 format (' openmp_pi = ', f20.16, /, 'difference = ', f20.16)
  call end_timing()
  stop

contains

  real (dp) function f(x)
    implicit none
    real (dp), intent (in) :: x

    f = 4.0_dp/(1.0_dp+x*x)
  end function

end program
