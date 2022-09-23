module circle_module

  use shape_module

  type, extends (shape_type) :: circle_type

    integer, private :: radius_

  contains

    procedure, pass (this) :: get_radius
    procedure, pass (this) :: set_radius
    procedure, pass (this) :: draw => draw_circle

  end type

  interface circle_type
    module procedure circle_type_constructor
  end interface

contains

  type (circle_type) function circle_type_constructor(x, y, radius)
    implicit none
    integer, intent (in) :: x
    integer, intent (in) :: y
    integer, intent (in) :: radius

    call circle_type_constructor%set_x(x)
    call circle_type_constructor%set_y(y)
    circle_type_constructor%radius_ = radius
  end function

  integer function get_radius(this)
    implicit none
    class (circle_type), intent (in) :: this

    get_radius = this%radius_
  end function

  subroutine set_radius(this, radius)
    implicit none
    class (circle_type), intent (inout) :: this
    integer, intent (in) :: radius

    this%radius_ = radius
  end subroutine

  subroutine draw_circle(this)
    implicit none
    class (circle_type), intent (in) :: this

    print *, ' x = ', this%get_x()
    print *, ' y = ', this%get_y()
    print *, ' radius = ', this%radius_
  end subroutine

end module
