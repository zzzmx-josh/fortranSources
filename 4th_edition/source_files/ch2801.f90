include 'ch2801_shape_module.f90'

program ch2801
  use shape_module
  implicit none
  type (shape_type) :: s1 = shape_type(10, 20)
  integer :: x1 = 100
  integer :: y1 = 200

  print *, ' get '
  print *, s1%get_x(), ' ', s1%get_y()
  print *, ' draw '
  call s1%draw()
  print *, ' moveto '
  call s1%moveto(x1, y1)
  print *, ' draw '
  call s1%draw()
  print *, ' set '
  call s1%set_x(99)
  call s1%set_y(99)
  print *, ' draw'
  call s1%draw()
end program ch2801
