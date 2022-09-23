program ch1806
  implicit none
  integer :: allocate_status = 0
  integer, parameter :: n1 = 10000000
  integer, parameter :: n2 = 5
  integer, dimension (:), pointer :: x
  integer, dimension (1:n2), target :: y
  integer :: i

  do
    allocate (x(1:n1), stat=allocate_status)
    if (allocate_status>0) then
      print *, ' allocate failed. program ends.'
      stop
    end if
    do i = 1, n1
      x(i) = i
    end do
    do i = 1, n2
      print *, x(i)
    end do
    do i = 1, n2
      y(i) = i*i
    end do
    do i = 1, n2
      print *, y(i)
    end do
    x => y !                        x now points to y
    do i = 1, n2
      print *, x(i)
    end do
!   what has happened to the memory that x
!   used to point to?
  end do
end program
