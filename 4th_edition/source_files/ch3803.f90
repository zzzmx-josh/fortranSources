include 'precision_module.f90'
include 'timing_module.f90'

program ch3803

  use precision_module
  use timing_module

  implicit none
  integer, parameter :: n = 100000000
  character *12 :: nn = '100,000,000'
  character *80 :: report_file_name = 'ch3803_report.txt'

  real (dp), allocatable, dimension (:) :: x_dp

  integer :: allocate_status = 0
  integer :: ifail = 0

  character *20, dimension (5) :: heading1 = [ '  32 bit real', '  32 bit int ', '  64 bit real', '  64 bit int ', &
    ' 128 bit real' ]

  character *20, dimension (3) :: heading2 = [ '      Allocate ', '      Random   ', '      Sort     ' ]

  print *, 'Program starts'
  print *, 'N = ', nn
  call start_timing()

  open (unit=100, file=report_file_name)

100 format (a20, 2x, f8.3)
110 format (5(2x,e14.6))
120 format (5(2x,i10))

  print *, heading1(3)

  allocate (x_dp(1:n), stat=allocate_status)
  if (allocate_status/=0) then
    print *, 'Allocate failed. Program terminates'
    stop 30
  end if

  print 100, heading2(1), time_difference()
  call random_number(x_dp)
  print 100, heading2(2), time_difference()
  call m01caf(x_dp, 1, n, 'A', ifail)
  if (ifail/=0) then
    print *, 'sort failed. Program terminates'
    stop 100
  end if

  print 100, heading2(3), time_difference()
  write (unit=100, fmt='(a)') 'First 10 64 bit reals'
  write (unit=100, fmt=110) x_dp(1:10)

  close (200)
  print *, 'Program terminates'
  call end_timing()

end program
