module c_interop_module

  use iso_c_binding

  implicit none

  integer, parameter :: n = 80

contains

  function f_to_c(fortran_string)
    implicit none
    character (len=n, kind=c_char) :: f_to_c
    character (len=n) :: fortran_string
    integer :: f_length

    f_length = len_trim(fortran_string)
    if (f_length>=n) then
      f_length = 79
    end if
    f_to_c = fortran_string(1:f_length) // c_null_char
  end function

  function c_to_f(c_string)
    implicit none
    character (len=n) :: c_to_f
    character (len=n, kind=c_char) :: c_string
    integer :: c_length
    integer :: i

    c_length = 1
    c_to_f = ' '
    do i = 1, n
      if (c_string(i:i)==c_null_char) exit
      c_length = c_length + 1
    end do
    c_length = c_length - 1
    c_to_f = c_string(1:c_length)
  end function

end module
