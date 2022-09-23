include 'precision_module.f90'

module ge_module
  use precision_module, wp => dp
  implicit none

contains
  subroutine gaussian_elimination(a, n, b, x, singular)

!   routine to solve a system ax=b
!   using gaussian elimination
!   with partial pivoting
!   the code is based on the linpack routines
!   sgefa and sgesl
!   and operates on columns rather than rows!

    implicit none

!   matrix a and vector b are over-written
!   arguments

    integer, intent (in) :: n
    real (wp), intent (inout) :: a(:, :), b(:)
    real (wp), intent (out) :: x(:)
    logical, intent (out) :: singular

!   local variables

    integer :: i, j, k, pivot_row
    real (wp) :: pivot, sum, element
    real (wp), parameter :: eps = 1.e-13_wp

!   work through the matrix column by column

    do k = 1, n - 1

!     find largest element in column k for pivot
!

      pivot_row = maxval(maxloc(abs(a(k:n,k)))) + k - 1

!     test to see if a is singular
!     if so return to main program

      if (abs(a(pivot_row,k))<=eps) then
        singular = .true.
        return
      else
        singular = .false.
      end if

!     exchange elements in column k if largest
!     is
!     not on the diagonal

      if (pivot_row/=k) then
        element = a(pivot_row, k)
        a(pivot_row, k) = a(k, k)
        a(k, k) = element
        element = b(pivot_row)
        b(pivot_row) = b(k)
        b(k) = element
      end if

!     compute multipliers
!     elements of column k below diagonal
!     are set to these multipliers for use
!     in elimination later on

      a(k+1:n, k) = a(k+1:n, k)/a(k, k)

!     row elimination performed by columns for
!     efficiency

      do j = k + 1, n
        pivot = a(pivot_row, j)
        if (pivot_row/=k) then

!         swap if pivot row is not k

          a(pivot_row, j) = a(k, j)
          a(k, j) = pivot
        end if
        a(k+1:n, j) = a(k+1:n, j) - pivot*a(k+1:n, k)
      end do

!     apply same operations to b

      b(k+1:n) = b(k+1:n) - a(k+1:n, k)*b(k)
    end do

!   backward substitution

    do i = n, 1, -1
      sum = 0.0
      do j = i + 1, n
        sum = sum + a(i, j)*x(j)
      end do
      x(i) = (b(i)-sum)/a(i, i)
    end do
  end subroutine
end module

program ch2604
  use ge_module
  implicit none
  integer :: i, n
  real (wp), allocatable :: a(:, :), b(:), x(:)
  logical :: singular

  print *, 'number of equations?'
  read *, n
  allocate (a(1:n,1:n), b(1:n), x(1:n))
  do i = 1, n
    print *, 'input elements of row ', i, ' of a'
    read *, a(i, 1:n)
    print *, 'input element ', i, ' of b'
    read *, b(i)
  end do
  call gaussian_elimination(a, n, b, x, singular)
  if (singular) then
    print *, 'matrix is singular'
  else
    print *, 'solution x:'
    print *, x(1:n)
  end if
end program
