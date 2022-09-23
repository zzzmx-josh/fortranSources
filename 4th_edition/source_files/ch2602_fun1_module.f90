module fun1_module
  implicit none
contains
  subroutine fun1(t, y, f, n)
    use precision_module, wp => dp
    implicit none
    real (wp), intent (in), dimension (:) :: y
    real (wp), intent (out), dimension (:) :: f
    real (wp), intent (in) :: t
    integer, intent (in) :: n

    f(1) = tan(y(3))
    f(2) = -0.032_wp*f(1)/y(2) - 0.02_wp*y(2)/cos(y(3))
    f(3) = -0.032_wp/(y(2)*y(2))
  end subroutine
end module
