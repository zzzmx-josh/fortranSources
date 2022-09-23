program ch1807
  implicit none
  integer, pointer :: a => null(), b => null()
  integer, target :: c
  integer :: d

  allocate (a)
  allocate (b)
  a = 100
  b = 200
  print *, a, b
  print *, loc(a)
  print *, loc(b)
  print *, loc(c)
  print *, loc(d)
  c = 1
  a => c
  c = 2
  b => c
  d = a + b
  print *, a, b, c, d
  print *, loc(a)
  print *, loc(b)
  print *, loc(c)
  print *, loc(d)
end program
