module shape_module

  type shape_type

    integer :: x_ = 0
    integer :: y_ = 0

  contains

    procedure, pass (this) :: get_x
    procedure, pass (this) :: get_y
    procedure, pass (this) :: set_x
    procedure, pass (this) :: set_y
    procedure, pass (this) :: moveto
    procedure, pass (this) :: draw

  end type

contains

  include 'shape_module_include_code.f90'

end module
