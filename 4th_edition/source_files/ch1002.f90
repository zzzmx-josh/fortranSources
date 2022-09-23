program ch1002
  implicit none
  integer, parameter :: n = 10
  real, dimension (1:n) :: h
  real, dimension (1:n) :: w
  real, dimension (1:n) :: bmi
  integer :: i

  open (unit=100, file='ch1001.out', status='old')
  do i = 1, n
    read (100, fmt='(1x,f5.2, 2x, f4.1)') h(i), w(i)
  end do
  close (100)
  bmi = w/(h*h)
  do i = 1, n
    write (unit=*, fmt='(1x,f4.1)') bmi(i)
  end do
end program
