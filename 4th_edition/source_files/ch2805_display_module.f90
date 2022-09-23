module display_module

contains

  subroutine display(n_shapes, shape_array)
    use shape_wrapper_module
    implicit none
    integer, intent (in) :: n_shapes
    type (shape_wrapper), dimension (n_shapes) :: shape_array
    integer :: i

    do i = 1, n_shapes
      call shape_array(i)%x%draw()
    end do
  end subroutine

end module
