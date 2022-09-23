include 'precision_module.f90'
include 'ch2702_ragged_module.f90'

program ch2702

  use precision_module
  use ragged_module
  implicit none
  integer, parameter :: wp = sp
  integer :: i
  integer, parameter :: n = 3

  type (ragged(wp)), dimension (1:n) :: lower_diag

  do i = 1, n
    allocate (lower_diag(i)%ragged_row(1:i))
    print *, ' type in the values for row ', i
    read *, lower_diag(i)%ragged_row(1:i)
  end do
  do i = 1, n
    print *, lower_diag(i)%ragged_row(1:i)
  end do

end program
