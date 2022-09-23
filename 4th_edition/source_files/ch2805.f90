include 'ch2805_shape_module.f90'
include 'ch2804_circle_module.f90'
include 'ch2804_rectangle_module.f90'
include 'ch2805_shape_wrapper_module.f90'
include 'ch2805_display_module.f90'

program ch2805
  use shape_module
  use circle_module
  use rectangle_module
  use shape_wrapper_module
  use display_module
  implicit none
  integer, parameter :: n = 6
  integer :: i
  type (shape_wrapper), dimension (n) :: s

  s(1)%x = shape_type(10, 20)
  s(2)%x = circle_type(100, 200, 300)
  s(3)%x = rectangle_type(1000, 2000, 3000, &
    4000)
  s(4)%x = s(1)%x
  s(5)%x = s(2)%x
  s(6)%x = s(3)%x
  print *, ' calling display subroutine'
  call display(n, s)
  print *, ' select type with get methods'
  do i = 1, n
    select type (t=>s(i)%x)
    class is (shape_type)
      print *, ' x = ', t%get_x(), ' y = ', &
        t%get_y()
    class is (circle_type)
      print *, ' x = ', t%get_x(), ' y = ', &
        t%get_y()
      print *, ' radius = ', t%get_radius()
    class is (rectangle_type)
      print *, ' x = ', t%get_x(), ' y = ', &
        t%get_y()
      print *, ' height = ', t%get_height()
      print *, ' width = ', t%get_width()
    class default
      print *, ' do nothing'
    end select
  end do
  print *, ' select type with set methods'
  do i = 1, n
    select type (t=>s(i)%x)
    class is (shape_type)
      call t%set_x(19)
      call t%set_y(19)
    class is (circle_type)
      call t%set_x(199)
      call t%set_y(199)
      call t%set_radius(199)
    class is (rectangle_type)
      call t%set_x(1999)
      call t%set_y(1999)
      call t%set_height(1999)
      call t%set_width(1999)
    class default
      print *, ' do nothing'
    end select
  end do
  print *, ' calling display subroutine'
  call display(n, s)
end program ch2805
