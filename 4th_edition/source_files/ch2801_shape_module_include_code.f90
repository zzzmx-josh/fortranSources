!start shape_module_common_code
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

subroutine moveto(this, newx, newy)
  implicit none
  class (shape_type), intent (inout) :: this
  integer, intent (in) :: newx
  integer, intent (in) :: newy

  this%x_ = newx
  this%y_ = newy
end subroutine

subroutine draw(this)
  implicit none
  class (shape_type), intent (in) :: this

  print *, ' x = ', this%x_
  print *, ' y = ', this%y_
end subroutine
!end shape_module_common_code
