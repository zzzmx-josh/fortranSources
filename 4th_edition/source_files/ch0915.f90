program ch0915
  implicit none
  integer, parameter :: nrow = 5
  integer, parameter :: ncol = 6
  real, dimension (1:nrow*ncol) :: results = (/ 50, 47, 28, 89, 30, 46, 37, 67, 34, 65, 68, 98, 25, 45, 26, 48, 10, 36, 89, 56, &
    33, 45, 30, 65, 68, 78, 38, 76, 98, 65 /)
  real, dimension (1:nrow, 1:ncol) :: exam_results = 0.0
  real, dimension (1:nrow) :: people_average = 0.0
  real, dimension (1:ncol) :: subject_average = 0.0
  integer :: r, c

  exam_results = reshape(results, (/nrow,ncol/), (/0.0,0.0/), (/2,1/) )
  exam_results(1:nrow, 3) = 2.5*exam_results(1:nrow, 3)
  subject_average = sum(exam_results, dim=1)
  people_average = sum(exam_results, dim=2)
  people_average = people_average/ncol
  subject_average = subject_average/nrow
  do r = 1, nrow
    print 100, (exam_results(r,c), c=1, ncol), people_average(r)
  end do
  print *, '  ====  ====  ====  ====  ====  ===='
  print 110, subject_average(1:ncol)
100 format (1x, 6(1x,f5.1), ' = ', f6.2)
110 format (1x, 6(1x,f5.1))
end program
