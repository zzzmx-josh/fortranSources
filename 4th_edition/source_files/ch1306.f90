module etox_module
  implicit none

contains
  real function etox(x)
    implicit none
    real :: term
    real, intent (in) :: x
    integer :: nterm
    real, parameter :: tol = 1.0e-6

    etox = 1.0
    term = 1.0
    nterm = 0
    do
      nterm = nterm + 1
      term = (x/nterm)*term
      etox = etox + term
      if (abs(term)<=tol) exit
    end do
  end function
end module

program ch1306
  use etox_module
  implicit none
  real, parameter :: x = 1.0
  real :: y

  print *, ' Fortran intrinsic ', exp(x)
  y = etox(x)
  print *, ' User defined etox ', y
end program
