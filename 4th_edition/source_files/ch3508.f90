program ch3508
  use iso_c_binding
  interface
    subroutine reciprocal(nr, nc, x, y) bind (c, name='reciprocal')
      use iso_c_binding
      integer (c_int), value :: nr
      integer (c_int), value :: nc
      real (c_float), dimension (nr, nc) :: x
      real (c_float), dimension (nr, nc) :: y
    end subroutine
  end interface
  integer, parameter :: nr = 2
  integer, parameter :: nc = 6
  integer :: i
  real, dimension (nr, nc) :: x
  real, dimension (nr, nc) :: y
  real, dimension (nr*nc) :: t = [ (i,i=1,nr*nc) ]
  integer :: r
  integer :: c

  x = reshape(t, (/nr,nc/), order=(/2,1/) )
  print *, ' Fortran calling C'
  print *, ' two d array as parameter'
  print *, ' using C 99 VLA'
  do r = 1, nr
    print 100, x(r, 1:nc)
100 format (10(f5.1))
  end do
  call reciprocal(nr, nc, x, y)
  do r = 1, nr
    print 110, y(r, 1:nc)
110 format (10(f6.3))
  end do
end program
