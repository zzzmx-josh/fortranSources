program ch0514
!
! use the bit functions in Fortran to write out
! a
! 32 bit integer number equivalenced to a real
! using the transfer intrinsic as a sequence of
! zeros and ones
!
  implicit none
  integer :: i, j
  character (len=32) :: i_in_bits = ' '
  real :: x = 1.0

  print *, '         1         2         3'
  print *, '12345678901234567890123456789012'
  print *, i_in_bits
  i = transfer(x, i)
  do j = 0, 31
    if (btest(i,j)) then
      i_in_bits(32-j:32-j) = '1'
    else
      i_in_bits(32-j:32-j) = '0'
    end if
  end do
  print *, x
  print *, i_in_bits
end program
