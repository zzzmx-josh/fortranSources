module reciprocal_module

contains
  real elemental function reciprocal(a)
    implicit none
    real, intent (in) :: a

    reciprocal = 1.0/a
  end function
end module

program ch1213
  use reciprocal_module
  implicit none
  real :: x = 10.0
  real, dimension (5) :: y = [ 1.0, 2.0, 3.0, 4.0, 5.0 ]

  print *, ' reciprocal of x is ', reciprocal(x)
  print *, ' reciprocal of y is ', reciprocal(y)
end program
