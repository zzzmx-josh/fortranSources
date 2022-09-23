program ch0513
!
! use the bit functions in Fortran to write out
! a
! 32 bit integer number as a sequence of
! zeros and ones
!
  implicit none
  integer :: j
  integer :: i
  integer, parameter :: i8 = selected_int_kind(2)
  integer, parameter :: i16 = selected_int_kind(4)
  integer, parameter :: i32 = selected_int_kind(9)

  integer (i8) :: i1
  integer (i16) :: i2
  integer (i32) :: i3
  character (len=32) :: i_in_bits

  print *, ' type in an integer '
  read *, i
  i1 = int(i, kind(2))
  i2 = int(i, kind(4))
  i3 = int(i, kind(9))
  i_in_bits = ' '
  do j = 0, 7
    if (btest(i1,j)) then
      i_in_bits(8-j:8-j) = '1'
    else
      i_in_bits(8-j:8-j) = '0'
    end if
  end do
  print *, '         1         2         3'
  print *, '12345678901234567890123456789012'
  print *, i1
  print *, i_in_bits
  do j = 0, 15
    if (btest(i2,j)) then
      i_in_bits(16-j:16-j) = '1'
    else
      i_in_bits(16-j:16-j) = '0'
    end if
  end do
  print *, i2
  print *, i_in_bits
  do j = 0, 31
    if (btest(i3,j)) then
      i_in_bits(32-j:32-j) = '1'
    else
      i_in_bits(32-j:32-j) = '0'
    end if
  end do
  print *, i3
  print *, i_in_bits
end program
