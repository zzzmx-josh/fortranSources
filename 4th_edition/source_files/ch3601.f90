include 'precision_module.f90'

program ch3601

  use precision_module
  use ieee_arithmetic

  implicit none

  real (sp) :: x = 1.0
  real (dp) :: y = 1.0_dp
  real (qp) :: z = 1.0_qp

  if (ieee_support_datatype(x)) then
    print *, '  32 bit IEEE support'
  end if
  if (ieee_support_datatype(y)) then
    print *, '  64 bit IEEE support'
  end if
  if (ieee_support_datatype(z)) then
    print *, ' 128 bit IEEE support'
  end if

end program
