program ch1205
  implicit none
  real, dimension (5) :: x = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)

  print *, ' dot product of x with x is'
  print *, ' ', dot_product(x, x)
end program
