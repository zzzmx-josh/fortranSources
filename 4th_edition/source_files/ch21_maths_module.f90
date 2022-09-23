module maths_module

  use precision_module, wp => dp

  implicit none

  real (wp), parameter :: c = 299792458.0_wp
! units m s-1

  real (wp), parameter :: e = 2.7182818284590452353602874713526624977_wp

  real (wp), parameter :: g = 9.812420_wp
! 9.780 356 m s-2 at sea level on the equator
! 9.812 420 m s-2 at sea level in london
! 9.832 079 m s-2 at sea level at the poles

  real (wp), parameter :: pi = 3.141592653589793238462643383279502884_wp

end module
