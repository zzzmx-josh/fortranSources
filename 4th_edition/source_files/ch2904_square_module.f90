module square_module

  use shape_module

  type, extends (shape_type) :: square_type

    integer, private :: side_ = 0

  contains

    procedure, pass (this) :: area => square_area

  end type

  interface square_type
    module procedure square_type_constructor
  end interface

contains

  type (square_type) function square_type_constructor(x, y, side)
    implicit none
    integer, intent (in) :: x
    integer, intent (in) :: y
    integer, intent (in) :: side

    call square_type_constructor%set_x(x)
    call square_type_constructor%set_y(y)
    square_type_constructor%side_ = side

  end function

  integer function square_area(this)
    implicit none
    class (square_type), intent (in) :: this

    square_area = this%side_*this%side_
  end function

end module
