module rkm_module

interface
  
  module subroutine runge_kutta_merson(y, fun, ifail, n, a, b, tol)

  use precision_module, wp=> dp

    implicit none

    real (wp), intent (inout), dimension (:) :: y
    real (wp), intent (in) :: a, b, tol
    integer, intent (in) :: n
    integer, intent (out) :: ifail

    interface

      subroutine fun(t, y, f, n)
        use precision_module, wp => dp
        implicit none
        real (wp), intent (in), dimension (:) :: y
        real (wp), intent (out), dimension (:) :: f
        real (wp), intent (in) :: t
        integer, intent (in) :: n
      end subroutine fun

    end interface

   end subroutine runge_kutta_merson

end interface

end module rkm_module
