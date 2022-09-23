module md_module
  implicit none

contains
  subroutine matrix_diagonal(a, diag, n)
    implicit none
    real, intent (in), dimension (:, :) :: a
    real, intent (out), dimension (:) :: diag
    integer, intent (in) :: n
    real, dimension (1:size(a,1)*size(a,1)) :: temp

!   subroutine to extract the diagonal
!   elements of an n * n matrix A

    temp = pack(a, .true.)
    diag = temp(1:n*n:n+1)
  end subroutine
end module

program ch2603
! program reads the n * n matrix from a file
  use md_module
  implicit none
  integer :: i, n
  real, allocatable, dimension (:, :) :: a
  real, allocatable, dimension (:) :: adiag
  character (len=20) :: filename

  print *, 'input name of data file'
  read '(a)', filename
  open (unit=1, file=filename, status='old')
  read (1, *) n
  allocate (a(1:n,1:n), adiag(1:n))
  do i = 1, n
    read (1, *) a(i, 1:n)
  end do
  call matrix_diagonal(a, adiag, n)
  print *, ' diagonal elements of a are:'
  print *, adiag
end program
