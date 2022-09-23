program ch0912
  implicit none
  integer :: fluid
  real :: litres
  real :: pints

  open (unit=1, file='ch0912.txt')
  write (unit=1, fmt=100)
  do fluid = 1, 10
    litres = fluid/1.75
    pints = fluid*1.75
    write (unit=1, fmt=110) pints, fluid, litres
  end do
  close (1)
100 format ('   Pints       Litres')
110 format (' ', f7.3, ' ', i3, ' ', f7.3)
end program
