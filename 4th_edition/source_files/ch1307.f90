program ch1307
  implicit none
  real :: hi, hr, hlow, high, half, xl
  real :: xh, xm, d
  real, parameter :: tol = 1.0e-6
! problem - find hi from expression given
! in function f
! F=A*(1.0-0.8*EXP(-0.6*C/A))-B
! The above is a Fortran 77
! statement function.
! hi is incident wave height (c)
! hr is reformed wave height (b)
! d is water depth at terrace edge (a)
  print *, ' Give reformed wave height, and water depth'
  read *, hr, d

! for hlow - let hlow=hr
! for high - let high=hlow*2.0

! check that signs of function
! results are different

  hlow = hr
  high = hlow*2.0
  xl = f(hlow, hr, d)
  xh = f(high, hr, d)

  do while ((xl*xh)>=0.0)
    high = high*2.0
    xh = f(high, hr, d)
  end do

  do
    half = (hlow+high)*0.5
    xm = f(half, hr, d)
    if ((xl*xm)<0.0) then
      xh = xm
      high = half
    else
      xl = xm
      hlow = half
    end if
    if (abs(high-hlow)<=tol) exit
  end do
  print *, ' Incident Wave Height Lies Between'
  print *, hlow, ' and ', high, ' metres'
contains
  real function f(a, b, c)
    implicit none
    real, intent (in) :: a
    real, intent (in) :: b
    real, intent (in) :: c

    f = a*(1.0-0.8*exp(-0.6*c/a)) - b
  end function
end program
