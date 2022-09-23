function summation(x, n) bind (c, name='summation')
  use iso_c_binding
  implicit none
  integer (c_int), value :: n
  real (c_float), dimension (1:n), intent (in) :: x
  real (c_float) :: summation
  integer :: i

  summation = sum(x(1:n))
end function
