program ch3505
  use iso_c_binding
  interface
    function summation(x, n) bind (c, name='summation')
      use iso_c_binding
      integer (c_int), value :: n
      real (c_float), dimension (1:n), intent (in) :: x
      real (c_float) :: summation
    end function
  end interface
  integer, parameter :: n = 10
  real, dimension (1:n) :: x = 1.0

  print *, ' Fortran calling c function'
  print *, ' 1 d array as parameter'
  print *, summation(x, n)
end program
