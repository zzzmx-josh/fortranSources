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

program ch2101

  use precision_module, wp => dp
  use sub1_module

  implicit none

  real (wp) :: r, a, c

  print *, 'radius?'
  read *, r
  call sub1(r, a, c)
  print *, ' for radius = ', r
  print *, ' area = ', a
  print *, ' circumference = ', c

end program
