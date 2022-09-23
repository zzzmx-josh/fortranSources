program ch1802
  implicit none
  integer, pointer :: a => null(), b => null()
  integer, target :: c
  integer :: d

  print *, associated(a)
  print *, associated(b)
  c = 1
  a => c
  c = 2
  b => c
  d = a + b
  print *, a, b, c, d
  print *, associated(a)
  print *, associated(b)
end program
