include 'precision_module.f90'
include 'ch2602_rkm_module.f90'
include 'ch2602_fun1_module.f90'

program ch2602
  use precision_module, wp => dp
  use rkm_module
  use fun1_module
  implicit none
  real (wp), dimension (:), allocatable :: y
  real (wp) :: a, b, tol
  integer :: n, ifail, all_stat

  print *, 'input no of equations'
  read *, n

! allocate space for y - checking to see that it
! allocates properly

  allocate (y(1:n), stat=all_stat)
  if (all_stat/=0) then
    print *, ' not enough memory'
    print *, ' array y is not allocated'
    stop
  end if
  print *, ' input start and end of interval over'
  print *, ' which equations to be solved'
  read *, a, b
  print *, 'input initial conditions'
  read *, y(1:n)
  print *, 'input tolerance'
  read *, tol
  print 100, a
100 format ('at t= ', f5.2, ' initial conditions are :')
  print 110, y(1:n)
110 format (4(f5.2,2x))
  call runge_kutta_merson(y, fun1, ifail, n, a, b, tol)
  if (ifail/=0) then
    print *, 'integration stopped with ifail = ', ifail
  else
    print 120, b
120 format ('at t= ', f5.2, ' solution is:')
    print 110, y(1:n)
  end if
end program
