program ch1801
  implicit none
  integer, pointer :: a => null(), b => null()
  integer, target :: c
  integer :: d

  c = 1
  a => c
  c = 2
  b => c
  d = a + b
  print *, a, b, c, d
end program
