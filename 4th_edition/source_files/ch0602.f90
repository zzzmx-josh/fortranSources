program ch0602
! The program reads up to number_of_people
! weights into the array Weight
! Variables used
! Weight, holds the weight of the people
! Person, an index into the array
! Total, total weight
! Average, average weight of the people
! Parameters used
! NumberOfPeople ,10 in this case.
! The weights are written out so that
! they can be checked
!
  implicit none
  integer, parameter :: number_of_people = 10
  real :: total = 0.0, average = 0.0
  integer :: person
  real, dimension (1:number_of_people) :: weight

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
