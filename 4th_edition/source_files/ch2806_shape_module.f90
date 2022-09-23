module shape_module

  type shape_type

    integer, private :: x_ = 0
    integer, private :: y_ = 0

  contains

    procedure, pass (this) :: get_x
    procedure, pass (this) :: get_y
    procedure, pass (this) :: set_x
    procedure, pass (this) :: set_y
    procedure, pass (this) :: moveto
    procedure, pass (this) :: draw

  end type

  interface shape_type
    module procedure shape_type_constructor
  end interface

contains

  type (shape_type) function shape_type_constructor(x, y)
    implicit none
    integer, intent (in) :: x
    integer, intent (in) :: y

    shape_type_constructor%x_ = x
    shape_type_constructor%y_ = y
  end function

  include 'shape_module_include_code.f90'

end module
