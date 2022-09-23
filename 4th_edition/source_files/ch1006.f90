program ch1006
  implicit none
  integer, parameter :: nrow = 5
  integer, parameter :: ncol = 6
  real, dimension (1:nrow, 1:ncol) :: exam_results = 0.0
  real, dimension (1:nrow) :: people_average = 0.0
  real, dimension (1:ncol) :: subject_average = 0.0
  integer :: r, c

  open (unit=100, file='ch1006.txt', status='old')
  do r = 1, nrow
    read (unit=100, fmt=100) exam_results(r, 1:ncol)
    people_average(r) = sum(exam_results(r,1:ncol))
  end do
  close (100)
  people_average = people_average/ncol
  do c = 1, ncol
    subject_average(c) = sum(exam_results(1:nrow,c))
  end do
  subject_average = subject_average/nrow
  do r = 1, nrow
    print 110, (exam_results(r,c), c=1, ncol), people_average(r)
  end do
  print *, '  ====  ====  ====  ====  ====  ===='
  print 120, subject_average(1:ncol)

100 format (1x, 6(1x,f5.1))
110 format (1x, 6(1x,f5.1), ' = ', f6.2)
120 format (1x, 6(1x,f5.1))
end program
