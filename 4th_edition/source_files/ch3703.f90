include 'timing_module.f90'
include 'ch3701_person_module.f90'

program ch3703

  use ch3701_person_module
  use timing_module

  implicit none

  integer :: i
  integer, parameter :: n = 1000000

  type (person) :: p1 = person('Zaphod Beeblebrox', 42, 1.85, 70)

  open (unit=10, file='ch3703.txt')

  call start_timing()

  do i = 1, n
    write (10, 100) p1%name, p1%age, p1%height, p1%weight
100 format (a39, 2x, i2, 2x, f4.2, 2x, f3.0)
  end do

  print 110, time_difference()
110 format (2x, f8.3)

  do i = 1, n
    write (10, 120) p1
120 format (dt)
  end do

  print 110, time_difference()

  close (10)

  call end_timing()

end program
