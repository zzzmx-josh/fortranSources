program ch0913
  implicit none
  integer, parameter :: n = 10000000
  integer, dimension (1:n) :: x = 0
  real, dimension (1:n) :: y = 0.0
  integer :: i
  real :: t, t1, t2, t3, t4, t5
  character *30 :: comment

  open (unit=10, file='ch0913.txt')
  call cpu_time(t)
  t1 = t
  comment = ' Program starts '
  print 120, comment, t1
  do i = 1, n
    x(i) = i
  end do
  call cpu_time(t)
  t2 = t - t1
  comment = ' Integer array initialised'
  print 120, comment, t2
  y = real(x)
  call cpu_time(t)
  t3 = t - t1 - t2
  comment = ' Real    array initialised'
  print 120, comment, t2
  do i = 1, n
    write (10, 100) x(i)
  end do
  call cpu_time(t)
  t4 = t - t1 - t2 - t3
  comment = ' Integer write '
  print 120, comment, t4
  do i = 1, n
    write (10, 110) y(i)
  end do
  call cpu_time(t)
  t5 = t - t1 - t2 - t3 - t4
  comment = ' Real    write '
  print 120, comment, t5
100 format (1x, i10)
110 format (1x, f10.0)
120 format (1x, a, 2x, f7.3)

end program
