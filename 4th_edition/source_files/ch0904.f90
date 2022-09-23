program ch0904
  implicit none
  integer :: fluid
  real :: litres
  real :: pints

  print *, ' Imperial     Litre(s)'
  print *, ' pint(s)              '
  do fluid = 1, 10
    litres = fluid/1.75
    pints = fluid*1.75
    print 100, pints, fluid, litres
  end do
100 format (' ', f6.2, '   ', i3, '   ', f5.2)
end program
