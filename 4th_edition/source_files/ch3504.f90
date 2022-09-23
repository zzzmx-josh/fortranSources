function reciprocal(x) bind (c, name='reciprocal')
  use iso_c_binding
  implicit none
  real (c_float), intent (in) :: x
  real (c_float) :: reciprocal

  reciprocal = 1.0/x
end function
