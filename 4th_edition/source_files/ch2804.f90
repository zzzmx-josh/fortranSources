include 'ch2803_shape_module.f90'
include 'ch2804_circle_module.f90'
include 'ch2804_rectangle_module.f90'

program ch2804

  use shape_module
  use circle_module
  use rectangle_module

  implicit none

  type (shape_type) :: vs
  type (circle_type) :: vc
  type (rectangle_type) :: vr

  vs = shape_type(10, 20)
  vc = circle_type(100, 200, 300)
  vr = rectangle_type(1000, 2000, 3000, 4000)
  print *, ' get '
  print *, ' shape     ', vs%get_x(), ' ', &
    vs%get_y()
  print *, ' circle    ', vc%get_x(), ' ', &
    vc%get_y(), 'radius = ', vc%get_radius()
  print *, ' rectangle ', vr%get_x(), ' ', &
    vr%get_y(), 'width = ', vr%get_width(), &
    'height ', vr%get_height()
  print *, ' draw '
  call vs%draw()
  call vc%draw()
  call vr%draw()
  print *, ' set '
  call vs%set_x(19)
  call vs%set_y(19)
  call vc%set_x(199)
  call vc%set_y(199)
  call vc%set_radius(199)
  call vr%set_x(1999)
  call vr%set_y(1999)
  call vr%set_width(1999)
  call vr%set_height(1999)
  print *, ' draw '
  call vs%draw()
  call vc%draw()
  call vr%draw()
end program ch2804
