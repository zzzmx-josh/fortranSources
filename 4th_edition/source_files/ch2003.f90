module matrix_module
  implicit none

contains
  subroutine matrix_bits(a, b, c, a_t, n)
    implicit none
    integer, intent (in) :: n
    real, dimension (:, :), intent (in) :: a, b
    real, dimension (:, :), intent (out) :: c, a_t
    integer :: i, j, k
    real :: temp
!   matrix multiplication c=ab
    do i = 1, n
      do j = 1, n
        temp = 0.0
        do k = 1, n
          temp = temp + a(i, k)*b(k, j)
        end do
        c(i, j) = temp
      end do
    end do
!   calculate a_t transpose of a
!   set a_t to be transpose matrix a
    do i = 1, n
      do j = 1, n
        a_t(i, j) = a(j, i)
      end do
    end do
  end subroutine
end module

program ch2003
  use matrix_module
  implicit none
  real, allocatable, dimension (:, :) :: one, two, three, one_t
  integer :: i, n

  print *, 'input size of matrices'
  read *, n
  allocate (one(1:n,1:n))
  allocate (two(1:n,1:n))
  allocate (three(1:n,1:n))
  allocate (one_t(1:n,1:n))
  do i = 1, n
    print *, 'input row ', i, ' of one'
    read *, one(i, 1:n)
  end do
  do i = 1, n
    print *, 'input row ', i, ' of two'
    read *, two(i, 1:n)
  end do
  call matrix_bits(one, two, three, one_t, n)
  print *, ' matrix three:'
  do i = 1, n
    print *, three(i, 1:n)
  end do
  print *, ' matrix one_t:'
  do i = 1, n
    print *, one_t(i, 1:n)
  end do
end program
