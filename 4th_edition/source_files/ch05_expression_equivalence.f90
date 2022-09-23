program expression_equivalence
!
! simple evaluation of x*x-y*y
! when x and y are similar
!
! we will evaluate in three ways.
!
  implicit none
  real :: x = 1.002
  real :: y = 1.001
  real :: t1, t2, t3, t4, t5

  t1 = x - y
  t2 = x + y
  print *, t1
  print *, t2
  t3 = t1*t2
  t4 = x**2 - y**2
  t5 = x*x - y*y
  print *, t3
  print *, t4
  print *, t5
end program
