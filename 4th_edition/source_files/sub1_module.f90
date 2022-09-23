module sub1_module
  implicit none

contains

  subroutine sub1(radius, area, circumference)

    use precision_module, wp => dp
    use maths_module
    implicit none
    real (wp), intent (in) :: radius
    real (wp), intent (out) :: area, circumference

    area = pi*radius*radius
    circumference = 2.0_wp*pi*radius

  end subroutine

end module
