program ch0806
  implicit none
!
! 1 us gallon = 3.7854118 litres
! 1 uk gallon = 4.545 litres
!
  integer, parameter :: n = 10
  real, parameter :: us = 3.7854118
  real, parameter :: uk = 4.545
  integer :: i
  integer, dimension (1:n) :: litre = [ (i,i=1,n) ]
  real, dimension (1:n) :: gallon, usgallon

  gallon = litre/uk
  usgallon = litre/us
  print *, ' Litres Imperial USA'
  print *, ' Gallon Gallon'
  do i = 1, n
    print *, litre(i), ' ', gallon(i), ' ', usgallon(i)
  end do
end program
