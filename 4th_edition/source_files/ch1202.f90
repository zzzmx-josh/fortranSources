program ch1202
  implicit none
  complex :: c = cmplx(1.0, 1.0)
  real :: r = 10.9
  integer :: i = -27

  print *, abs(c)
  print *, abs(r)
  print *, abs(i)
end program
