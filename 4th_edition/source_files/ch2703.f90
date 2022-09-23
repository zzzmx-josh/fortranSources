include 'precision_module.f90'
include 'ch2703_matrix_module.f90'

program ch2703
  use precision_module
  use pdt_matrix_module

  implicit none

  real (sp) :: scs
  real (dp) :: scd
  integer, parameter :: nr = 2, nc = 3
  integer :: i
  type (pdt_matrix(sp,nr,nc)) :: as
  type (pdt_matrix(dp,nr,nc)) :: ad
!
! single precision
!
  do i = 1, nr
    print *, 'input row ', i, ' of sp matrix'
    read *, as%m(i, 1:nc)
  end do
  print *, 'input sp scaling factor'
  read *, scs
  call scale_matrix(as, scs)
  print *, 'updated matrix:'
  do i = 1, nr
    print 100, as%m(i, 1:nc)
100 format (10(f6.2,2x))
  end do
!
! double precision
!
  do i = 1, nr
    print *, 'input row ', i, ' of dp matrix'
    read *, ad%m(i, 1:nc)
  end do
  print *, 'input dp scaling factor'
  read *, scd
  call scale_matrix(ad, scd)
  print *, 'updated matrix:'
  do i = 1, nr
    print 110, ad%m(i, 1:nc)
110 format (10(e12.5,2x))
  end do

end program
