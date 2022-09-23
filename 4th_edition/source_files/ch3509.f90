subroutine reciprocal(nr, nc, x, y) bind (c, name='reciprocal')
  use iso_c_binding
  implicit none
  integer (c_int), value :: nr
  integer (c_int), value :: nc
  real (c_float), dimension (1:nr, 1:nc), intent (in) :: x
  real (c_float), dimension (1:nr, 1:nc), intent (out) :: y

  y = 1.0/x
end subroutine
