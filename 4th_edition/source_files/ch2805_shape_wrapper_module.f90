module shape_wrapper_module
  use shape_module
  use circle_module
  use rectangle_module
  type shape_wrapper

    class (shape_type), allocatable :: x
  end type
end module
