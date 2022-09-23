module pdt_matrix_module

  use precision_module
  implicit none

  type pdt_matrix(k, row, col)
    integer, kind :: k
    integer, len :: row, col
    real (kind=k), dimension (row, col) :: m
  end type

  interface scale_matrix
    module procedure scale_matrix_sp
    module procedure scale_matrix_dp
  end interface

contains

  subroutine scale_matrix_sp(a, scale)
    type (pdt_matrix(sp,*,*)), intent (inout) :: a
    real (sp) :: scale

    a%m = a%m + scale
  end subroutine

  subroutine scale_matrix_dp(a, scale)
    type (pdt_matrix(dp,*,*)), intent (inout) :: a
    real (dp) :: scale

    a%m = a%m + scale
  end subroutine

end module
