include 'precision_module.f90'

program ch3602

  use precision_module
  use ieee_arithmetic

  implicit none

  real (sp) :: x = 1.0
  real (dp) :: y = 1.0_dp
  real (qp) :: z = 1.0_qp

  integer :: i

  character *20, dimension (5) :: flags = (/ 'IEEE_DIVIDE_BY_ZERO ', 'IEEE_INEXACT        ', 'IEEE_INVALID        ', &
    'IEEE_OVERFLOW       ', 'IEEE_UNDERFLOW      ' /)

  do i = 1, 5
    if (ieee_support_flag(ieee_all(i),x)) then
      write (unit=*, fmt=100) flags(i)
100   format (a20, ' 32 bit support')
    end if
    if (ieee_support_flag(ieee_all(i),y)) then
      write (unit=*, fmt=110) flags(i)
110   format (a20, ' 64 bit support')

    end if
    if (ieee_support_flag(ieee_all(i),z)) then
      write (unit=*, fmt=120) flags(i)
120   format (a20, '128 bit support')
    end if
  end do

end program
