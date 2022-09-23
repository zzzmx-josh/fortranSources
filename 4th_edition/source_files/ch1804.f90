program ch1804
  implicit none
  integer, pointer :: a => null(), b => null()
  integer, target :: c
  integer :: d

  allocate (a)
  a = 1
  c = 2
  b => c
  d = a + b
  print *, a, b, c, d
  deallocate (a)
end program
