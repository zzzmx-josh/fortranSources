program ch3502
  use iso_c_binding
  interface
    real (c_float) function reciprocal(x) bind (c, name='reciprocal')
      use iso_c_binding
      real (c_float), value :: x
    end function
  end interface
  real :: x

  x = 10.0
  print *, ' Fortran calling C function'
  print *, x, ' reciprocal = ', reciprocal(x)
end program
