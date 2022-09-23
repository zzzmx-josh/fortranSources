module etox_module
  implicit none

contains
  elemental real function etox(x)
    implicit none
    real, intent (in) :: x
    real :: term
    integer :: nterm
    real, parameter :: tol = 1.0e-6

    etox = 1.0
    term = 1.0
    nterm = 0
    do
      nterm = nterm + 1
      term = (x/nterm)*term
      etox = etox + term
      if (term<=tol) exit
    end do
  end function
end module
program ch2606
  use etox_module
  implicit none
  integer :: i
  real :: x
  real, dimension (10) :: y

  x = 1.0
  do i = 1, 10
    y(i) = i
  end do
  print *, y
  x = etox(x)
  print *, x
  y = etox(y)
  print *, y
end program
