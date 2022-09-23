include 'precision_module.f90'

include 'timing_module.f90'

program ch3403

  use precision_module
  use timing_module

  implicit none

  real (dp) :: fortran_internal_pi
  real (dp) :: partial_pi
  real (dp) :: coarray_pi
  real (dp) :: width
  real (dp) :: total_sum
  real (dp) :: x
  real (dp), codimension [ * ] :: partial_sum

  integer :: n_intervals
  integer :: i
  integer :: j
  integer :: current_image
  integer :: n_images

  fortran_internal_pi = 4.0_dp*atan(1.0_dp)
  n_images = num_images()
  current_image = this_image()

  if (current_image==1) then
    print *, ' Number of images = ', n_images
  end if

  n_intervals = 100000

  do j = 1, 5
    if (current_image==1) then
      call start_timing()
    end if
    width = 1.0_dp/real(n_intervals, dp)
    total_sum = 0.0_dp
    partial_sum = 0.0_dp
    do i = current_image, n_intervals, n_images
      x = (real(i,dp)-0.5_dp)*width
      partial_sum = partial_sum + f(x)
    end do
    partial_sum = partial_sum*width
    sync all
    if (current_image==1) then
      do i = 1, n_images
        total_sum = total_sum + partial_sum[ i ]
      end do
      coarray_pi = total_sum
      print 100, n_intervals, time_difference()
      print 110, coarray_pi, abs(coarray_pi-fortran_internal_pi)
    end if
    n_intervals = n_intervals*10
    sync all
  end do

100 format (' n intervals = ', i12, ' time =', f8.3)
110 format (' pi = ', f20.16, /, ' difference = ', f20.16)

contains

  real (dp) function f(x)
    implicit none
    real (dp), intent (in) :: x

    f = 4.0_dp/(1.0_dp+x*x)

  end function

end program
