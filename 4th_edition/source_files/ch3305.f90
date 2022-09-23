include 'timing_module.f90'
include 'precision_module.f90'

program ch3305

  use timing_module
  use precision_module
  use omp_lib

  implicit none

  integer, parameter :: n = 10000000
  integer, parameter :: loop_count = 10
  integer, parameter :: n_types = 4
  integer :: i
  integer :: j
  integer :: nthreads

  real (dp), allocatable, dimension (:) :: x
  real (dp), allocatable, dimension (:) :: y
  real (dp), allocatable, dimension (:) :: z

  real, dimension (n_types, loop_count) :: timing_details = 0.0
  real, dimension (n_types) :: t_sum = 0.0
  real, dimension (n_types) :: t_average = 0.0
  real :: reset = 0.0

  character (15), dimension (n_types) :: heading_1 = [ ' Whole array   ', ' Do loop       ', ' Do concurrent ', &
    ' openmp        ' ]

  call start_timing()
  print *, ' '

  nthreads = omp_get_max_threads()
  open (unit=20, file='ch3305.dat')
  print 100, nthreads
100 format (' Nthreads = ', i3)
  allocate (x(n))
  allocate (y(n))
  allocate (z(n))

  call random_number(x)
  call random_number(y)
  z = 0.0_dp
  print 110, time_difference()
110 format (' Initialise time = ', f6.3)
  write (20, 120) x(1), y(1), z(1)
120 format (3(2x,f6.3))
  print *, ' '

  do j = 1, loop_count

    print 130, j
130 format (' Iteration = ', i3)
!
!   Whole array syntax
!
    z = x + y
    timing_details(1, j) = time_difference()
    write (20, 120) x(1), y(1), z(1)
    z = 0.0_dp
    reset = time_difference()
!
!   Simple traditional do loop
!
    do i = 1, n
      z(i) = x(i) + y(i)
    end do
    timing_details(2, j) = time_difference()
    z = 0.0_dp
    reset = time_difference()
!
!   do concurrent loop
!
    do concurrent (i=1:n)
      z(i) = x(i) + y(i)
    end do
    timing_details(3, j) = time_difference()
    write (20, 120) x(1), y(1), z(1)
    z = 0.0_dp
    reset = time_difference()
!
!   OpenMP parallel loop
!
!$omp parallel do
    do i = 1, n
      z(i) = x(i) + y(i)
    end do
!$omp end parallel do
    timing_details(4, j) = time_difference()
    write (20, 120) x(1), y(1), z(1)
    z = 0.0_dp
    reset = time_difference()

  end do
  close (20)
  print 140
140 format (15x, 70x, '    Sum      Average')

  do i = 1, n_types
    t_sum(i) = sum(timing_details(i,1:loop_count))
    t_average(i) = t_sum(i)/loop_count
    print 150, heading_1(i), timing_details(i, 1:loop_count), t_sum(i), t_average(i)
150 format (a, 10(1x,f6.3), 2(3x,f6.3))
  end do

  print *, ' '
  call end_timing()
end program

