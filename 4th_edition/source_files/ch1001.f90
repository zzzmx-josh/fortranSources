program ch1001
  implicit none
  integer, parameter :: npeople = 10
  integer, dimension (1:npeople) :: height_feet, height_inch, weight_stone, weight_pound
  real, dimension (1:npeople) :: weight_kg, height_m
  integer :: i

  open (unit=10, file='ch1001.txt', status='old')
  open (unit=20, file='ch1001.out', status='new')

  do i = 1, npeople
    read (10, fmt=100) height_feet(i), height_inch(i), weight_stone(i), weight_pound(i)
100 format (i2, 2x, i2, 2x, i2, 2x, i2)
    weight_kg(i) = (weight_stone(i)*14+weight_pound(i))/2.2
    height_m(i) = (height_feet(i)*12+height_inch(i))*2.54/100
    write (unit=20, fmt=110) height_m(i), weight_kg(i)
110 format (1x, f5.2, 2x, f4.1)
  end do
  close (10)
  close (20)
end program

