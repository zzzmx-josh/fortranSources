program ch1501
  implicit none
  complex :: z, z1, z2, z3, zbar
  real :: x, y, zmod
  real :: x2 = 3.0, y2 = 4.0
  real :: x3 = -2.0, y3 = -3.0

  z1 = cmplx(1.0, 2.0) !            1 + i 2
  z2 = cmplx(x2, y2) !              x2 + i y2
  z3 = cmplx(x3, y3) !              x3 + i y3
  z = z1*z2/z3
  x = real(z) !                     real part of
! z
  y = aimag(z) !                    imaginary
! part of z
  zmod = abs(z) !                   modulus of z
  zbar = conjg(z) !                 complex
! conjugate of
! z
  print 100, z1, z2, z3
100 format (3(1x,f4.1,' + i ',f4.1,/))
  print 110, z, zmod, zbar
110 format (1x, f4.1, ' + i ', f4.1, /, 1x, f4.1, /, 1x, f4.1, ' + i ', f4.1)
  print 120, x, y
120 format (2(1x,f4.1,/))
end program
