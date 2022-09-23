program ch0708
  implicit none
!
! 1 us gallon = 3.7854118 litres
! 1 uk gallon = 4.545 litres
!
  integer :: litre
  real :: gallon, usgallon

  do litre = 1, 10
    gallon = litre/4.545
    usgallon = litre/3.7854118
    print *, litre, ' ', gallon, ' ', usgallon
  end do
end program
