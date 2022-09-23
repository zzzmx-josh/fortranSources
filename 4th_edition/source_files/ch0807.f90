program ch0807
  implicit none
  integer, dimension (1:3) :: x, y
  integer :: result

  x = [ 1, 3, 5 ]
  y = [ 2, 4, 6 ]
  result = dot_product(x, y)
  print *, result
end program
