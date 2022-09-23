include 'c_interop_module.f90'

program ch3513

  use iso_c_binding

  use c_interop_module

  implicit none

  interface

    subroutine print_string(x) bind (c, name='print_string')
      use iso_c_binding
      character (c_char) :: x(*)
    end subroutine

    subroutine replace_string(x) bind (c, name='replace_string')
      use iso_c_binding
      character (c_char) :: x(*)
    end subroutine

    subroutine concatenate_string(x) bind (c, name='concatenate_string')
      use iso_c_binding
      character (c_char) :: x(*)
    end subroutine

  end interface

  integer, parameter :: line_length = 80

  character (len=line_length) :: fortran_string
  character (len=line_length, kind=c_char) :: c_string

  fortran_string = 'Hello'
  c_string = f_to_c(fortran_string)

  print *, ' print_string '
  call print_string(c_string)

  fortran_string = 'Hello'
  c_string = f_to_c(fortran_string)

  print *, ' replace_string '
  call replace_string(c_string)
  fortran_string = c_to_f(c_string)
  print *, ' After              ', fortran_string

  fortran_string = 'Hello'
  c_string = f_to_c(fortran_string)

  print *, ' concatenate_string '
  call concatenate_string(c_string)
  fortran_string = c_to_f(c_string)
  print *, ' After              ', fortran_string

end program
