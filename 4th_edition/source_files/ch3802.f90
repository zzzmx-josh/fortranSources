include 'precision_module.f90'
include 'integer_kind_module.f90'
include 'timing_module.f90'

! Calls
! 
!   dsort.f
!   isort.f
!   ssort.f

program ch3802
  use precision_module
  use integer_kind_module
  use timing_module
  implicit none
  integer, parameter :: n = 100000000
  character *12 :: nn = '100,000,000'
  character *80 :: report_file_name = 'ch3802_report.txt'
  real (sp), allocatable, dimension (:) :: x_sp
  real (dp), allocatable, dimension (:) :: x_dp
  integer (i32), allocatable, dimension (:) :: y_i32

  integer :: allocate_status
  character *20, dimension (5) :: heading1 = [ '  32 bit real  ', '  32 bit int   ', '  64 bit real  ', '  64 bit int   ', &
    ' 128 bit real  ' ]

  character *20, dimension (3) :: heading2 = [ '      Allocate  ', '      Random    ', '      Sort      ' ]

  allocate_status = 0

  print *, 'Program starts'
  print *, 'N = ', nn
  call start_timing()

  open (unit=100, file=report_file_name)

  print *, heading1(1)

  allocate (x_sp(1:n), stat=allocate_status)
  if (allocate_status/=0) then
    print *, ' Allocate failed. Program terminates'
    stop 10
  end if
  print 100, heading2(1), time_difference()
100 format (a20, 2x, f8.3)
  call random_number(x_sp)
  print 100, heading2(2), time_difference()
  call ssort(x_sp, x_sp, n, 1)
  print 100, heading2(3), time_difference()
  write (unit=100, fmt='(a)') ' First 10 32 bit reals'
  write (unit=100, fmt=110) x_sp(1:10)
110 format (5(2x,e14.6))

  print *, heading1(2)

  allocate (y_i32(1:n), stat=allocate_status)
  if (allocate_status/=0) then
    print *, ' Allocate failed. Program terminates'
    stop 20
  end if
  print 100, heading2(1), time_difference()
  y_i32 = int(x_sp*1000000000, i32)
  deallocate (x_sp)
  print 100, heading2(2), time_difference()
  call isort(y_i32, y_i32, n, 1)
  print 100, heading2(3), time_difference()
  write (unit=100, fmt='(a)') 'First 10 32 bit integers'
  write (unit=100, fmt=120) y_i32(1:10)
120 format (5(2x,i10))
  deallocate (y_i32)

  print *, heading1(3)

  allocate (x_dp(1:n), stat=allocate_status)
  if (allocate_status/=0) then
    print *, ' Allocate failed. Program terminates'
    stop 30
  end if
  print 100, heading2(1), time_difference()
  call random_number(x_dp)
  print 100, heading2(2), time_difference()
  call dsort(x_dp, x_dp, n, 1)
  print 100, heading2(3), time_difference()
  write (unit=100, fmt='(a)') 'First 10 64 bit reals'
  write (unit=100, fmt=110) x_dp(1:10)

  print *, ' Program terminates'
  call end_timing()

end program

