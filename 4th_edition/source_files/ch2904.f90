include 'ch2904_abstract_shape_module.f90'
include 'ch2904_square_module.f90'

program ch2904

  use square_module

  type (square_type) :: x

  x = square_type(1, 2, 3)

  print *, ' Square area = ', x%area()

end program
