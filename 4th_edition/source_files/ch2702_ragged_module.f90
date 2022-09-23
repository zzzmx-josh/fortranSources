module ragged_module
  use precision_module
  implicit none
  type ragged(real_kind)
    integer, kind :: real_kind
    real (real_kind), dimension (:), allocatable :: ragged_row
  end type
end module
