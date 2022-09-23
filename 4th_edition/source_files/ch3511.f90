subroutine sums(nr, nc, x, rsum, csum) bind (c, name='sums')
! g++ needs -lgfortran to link
  use iso_c_binding
  implicit none
  integer (c_int), value :: nr
  integer (c_int), value :: nc
  integer (c_int), dimension (1:nr, 1:nc), intent (in) :: x
  integer (c_int), dimension (1:nr), intent (out) :: rsum
  integer (c_int), dimension (1:nc), intent (out) :: csum
  integer (c_int), dimension (1:nc, 1:nr) :: t

  t = reshape(x, (/nc,nr/) )
  rsum = sum(t, dim=1)
  csum = sum(t, dim=2)
end subroutine
