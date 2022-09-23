program ch1305
  implicit none
! this program picks up the first occurrence
! of a number in a list.
! a sentinel is used, and the array is 1 more
! than the max size of the list.
  integer, allocatable, dimension (:) :: a
  integer :: mark
  integer :: i, howmany

  open (unit=1, file='data.txt', status='old')
  print *, ' What number are you looking for?'
  read *, mark
  print *, ' How many numbers to search?'
  read *, howmany
  allocate (a(1:howmany+1))
  read (unit=1, fmt=*)(a(i), i=1, howmany)
  i = 1
  a(howmany+1) = mark
  do while (mark/=a(i))
    i = i + 1
  end do
  if (i==(howmany+1)) then
    print *, ' item not in list'
  else
    print *, ' item is at position ', i
  end if
end program
