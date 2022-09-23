program ch0402
!
! This program reads in three numbers and sums
! and averages them
!
  implicit none
  real :: n1, n2, n3, average = 0.0, total = 0.0
  integer :: n = 3

  print *, ' type in three numbers.'
  print *, ' Separated by spaces or commas'
  read *, n1, n2, n3
  total = n1 + n2 + n3
  average = total/n
  print *, 'Total of numbers is ', total
  print *, 'Average of the numbers is ', average
end program
