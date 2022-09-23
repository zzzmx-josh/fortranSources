module shape_module

  type, abstract :: shape_type

    integer, private :: x_ = 0
    integer, private :: y_ = 0

  contains

    procedure, pass (this) :: get_x
    procedure, pass (this) :: get_y
    procedure, pass (this) :: set_x
    procedure, pass (this) :: set_y

    procedure (calculate_area), pass (this), deferred :: area

  end type

  abstract interface
    integer function calculate_area(this)
      import :: shape_type
      class (shape_type), intent (in) :: this
    end function
  end interface

contains

  integer function get_x(this)
    implicit none

    class (shape_type), intent (in) :: this

    get_x = this%x_
  end function

  integer function get_y(this)
    implicit none

    class (shape_type), intent (in) :: this

    get_y = this%y_
  end function

  subroutine set_x(this, x)
    implicit none

    class (shape_type), intent (inout) :: this
    integer, intent (in) :: x

    this%x_ = x
  end subroutine

  subroutine set_y(this, y)
    implicit none

    class (shape_type), intent (inout) :: this
    integer, intent (in) :: y

    this%y_ = y
  end subroutine

end module
