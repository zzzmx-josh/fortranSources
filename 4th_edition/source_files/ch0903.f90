program ch0903
  implicit none
  integer :: fluid
  real :: imperial_pint
  real :: us_pint

  print *, ' US            Imperial'
  print *, ' pint(s)       pint(s)'
  do fluid = 1, 10
    imperial_pint = fluid*1.20095
    us_pint = fluid/1.20095
    print 100, imperial_pint, fluid, us_pint
100 format (' ', f5.2, '    ', i3, '    ', f5.2)
  end do
end program
