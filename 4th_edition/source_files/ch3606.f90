program ch3606

  use precision_module
  use ieee_arithmetic
  implicit none

  real (sp) :: x0 = 0.0
  real (dp) :: y0 = 0.0_dp
  real (qp) :: z0 = 0.0_qp

  real (sp) :: x1 = 1.0
  real (dp) :: y1 = 1.0_dp
  real (qp) :: z1 = 1.0_qp

  real (sp) :: xnan = 1.0
  real (dp) :: ynan = 1.0_dp
  real (qp) :: znan = 1.0_qp

  real (sp) :: xinfinite = 1.0
  real (dp) :: yinfinite = 1.0_dp
  real (qp) :: zinfinite = 1.0_qp

  xinfinite = x1/x0
  yinfinite = y1/y0
  zinfinite = z1/z0
  xnan = x0/x0
  ynan = y0/y0
  znan = z0/z0

  if (ieee_support_datatype(x1)) then
    print *, '  32 bit IEEE support'
    print *, '     inf ', ieee_support_inf(x1)
    print *, '     nan ', ieee_support_nan(x1)
    print *, ' 1/0 finite', ieee_is_finite(xinfinite)
    print *, ' 0/0 nan', ieee_is_nan(xnan)
  end if

  if (ieee_support_datatype(y1)) then
    print *, '  64 bit IEEE support'
    print *, '     inf ', ieee_support_inf(y1)
    print *, '     nan ', ieee_support_nan(y1)
    print *, ' 1/0 finite', ieee_is_finite(yinfinite)
    print *, ' 0/0 nan', ieee_is_nan(ynan)
  end if

  if (ieee_support_datatype(z1)) then
    print *, ' 128 bit IEEE support'
    print *, '     inf ', ieee_support_inf(z1)
    print *, '     nan ', ieee_support_nan(z1)
    print *, ' 1/0 finite', ieee_is_finite(zinfinite)
    print *, ' 0/0 nan', ieee_is_nan(znan)
  end if

end program
