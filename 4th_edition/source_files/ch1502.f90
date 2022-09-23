program ch1502
  implicit none

! program to calculate frequency
! response of a system
! for a given omega
! and its polar form (magnitude and phase).

  real :: omega, real_part, imag_part, magnitude, phase
  complex :: frequency_response

! Input frequency omega

  print *, 'Input frequency'
  read *, omega

  frequency_response = 1.0/cmplx(-omega*omega+1.0, 2.0*omega)
  real_part = real(frequency_response)
  imag_part = aimag(frequency_response)

! Calculate polar coordinates
! (magnitude and phase)

  magnitude = abs(frequency_response)
  phase = atan2(imag_part, real_part)

  print *, ' at frequency ', omega
  print *, 'response = ', real_part, ' + i ', imag_part
  print *, 'in polar form'
  print *, ' magnitude = ', magnitude
  print *, ' phase = ', phase
end program
