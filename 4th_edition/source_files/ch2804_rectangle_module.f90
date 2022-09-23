module rectangle_module

  use shape_module

  type, extends (shape_type) :: rectangle_type

    integer, private :: width_
    integer, private :: height_

  contains

    procedure, pass (this) :: get_width
    procedure, pass (this) :: set_width
    procedure, pass (this) :: get_height
    procedure, pass (this) :: set_height
    procedure, pass (this) :: draw => draw_rectangle

  end type

  interface rectangle_type
    module procedure rectangle_type_constructor
  end interface

contains

  type (rectangle_type) function rectangle_type_constructor(x, y, width, height)
    implicit none
    integer, intent (in) :: x
    integer, intent (in) :: y
    integer, intent (in) :: width
    integer, intent (in) :: height

    call rectangle_type_constructor%set_x(x)
    call rectangle_type_constructor%set_y(y)
    rectangle_type_constructor%width_ = width
    rectangle_type_constructor%height_ = height
  end function

  integer function get_width(this)
    implicit none
    class (rectangle_type), intent (in) :: this

    get_width = this%width_
  end function

  subroutine set_width(this, width)
    implicit none
    class (rectangle_type), intent (inout) :: this
    integer, intent (in) :: width

    this%width_ = width
  end subroutine

  integer function get_height(this)
    implicit none
    class (rectangle_type), intent (in) :: this

    get_height = this%height_
  end function

  subroutine set_height(this, height)
    implicit none
    class (rectangle_type), intent (inout) :: this
    integer, intent (in) :: height

    this%height_ = height
  end subroutine

  subroutine draw_rectangle(this)
    implicit none
    class (rectangle_type), intent (in) :: this

    print *, ' x = ', this%get_x()
    print *, ' y = ', this%get_y()
    print *, ' width = ', this%width_
    print *, ' height = ', this%height_
  end subroutine

end module
