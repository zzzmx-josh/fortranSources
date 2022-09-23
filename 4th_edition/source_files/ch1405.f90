program ch1405
  implicit none
  character (len=20) :: name
  integer :: name_length

  print *, ' type in your name'
  read '(a)', name
! show len first
  name_length = len(name)
  print *, ' name length is ', name_length
  print *, ' ', name(1:name_length), '<-end is here'
  name_length = len_trim(name)
  print *, ' name length is ', name_length
  print *, ' ', name(1:name_length), '<-end is here'
end program
