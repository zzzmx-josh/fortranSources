program ch1007
  implicit none
  integer :: ib1, ib2
  integer :: n1, n2
  character (len=22) :: buffer, buff1, buff2
! program to read a record of the form
! #xxxxxxxxxx yyyyyyyyyy
! so that integers n1 = xxxxxxxxxx n2 =
! yyyyyyyyyy
! where the number of digits varies from 1 to 10
!
! use internal files
  print *, 'input micael''s numbers'
  read (*, '(a)') buffer
  ib1 = index(buffer, ' ')
  ib2 = len_trim(buffer)
  buff1 = buffer(2:ib1-1)
  buff2 = buffer(ib1+1:ib2)
  read (buff1, '(i10)') n1
  read (buff2, '(i10)') n2
  print *, 'n1 = ', n1
  print *, 'n2 = ', n2
end program
