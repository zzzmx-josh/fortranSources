program ch0804
  implicit none
  integer, parameter :: nrow = 5
  integer, parameter :: ncol = 6
  real, dimension (1:nrow, 1:ncol) :: exam_results = 0.0
  real, dimension (1:nrow) :: people_average = 0.0
  real, dimension (1:ncol) :: subject_average = 0.0
  integer :: r, c

  do r = 1, nrow
    read *, exam_results(r, 1:ncol)
  end do
  exam_results(1:nrow, 3) = 2.5*exam_results(1:nrow, 3)
  do r = 1, nrow
    do c = 1, ncol
      people_average(r) = people_average(r) + exam_results(r, c)
    end do
  end do
  people_average = people_average/ncol
  do c = 1, ncol
    do r = 1, nrow
      subject_average(c) = subject_average(c) + exam_results(r, c)
    end do
  end do
  subject_average = subject_average/nrow
  print *, ' People averages'
  print *, people_average
  print *, ' Subject averages'
  print *, subject_average
end program
