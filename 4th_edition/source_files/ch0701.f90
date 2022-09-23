program ch0701
!
! This program is a simple variant of ch0602.
! The array is now allocatable
! and the user is prompted for the
! number of people at run time.
!
  implicit none
  integer :: number_of_people
  real :: total = 0.0, average = 0.0
  integer :: person
  real, dimension (:), allocatable :: weight

  print *, ' How many people?'
  read *, number_of_people
  allocate (weight(1:number_of_people))
  do person = 1, number_of_people
    print *, ' type in the weight for person ', person
    read *, weight(person)
    total = total + weight(person)
  end do
  average = total/number_of_people
  print *, ' The total of the weights is ', total
  print *, ' Average Weight is ', average
  print *, ' ', number_of_people, ' Weights were '
  do person = 1, number_of_people
    print *, weight(person)
  end do
end program
