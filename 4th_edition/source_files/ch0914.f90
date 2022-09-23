program ch0914
  implicit none
  integer, parameter :: n = 10000000
  integer, dimension (1:n) :: x = 0
  real, dimension (1:n) :: y = 0
  integer :: i
  real :: t, t1, t2, t3, t4, t5
  character *30 :: comment

  open (unit=10, file='ch0914.dat', form='unformatted')
  call cpu_time(t)
  t1 = t
  comment = ' Program starts '
  print 100, comment, t1
  do i = 1, n
    x(i) = i
  end do
  call cpu_time(t)
  t2 = t - t1
  comment = ' Integer assignment '
  print 100, comment, t2
  y = real(x)
  call cpu_time(t)
  t3 = t - t1 - t2
  comment = ' Real    assignment '
  print 100, comment, t2
  write (10) x
  call cpu_time(t)
  t4 = t - t1 - t2 - t3
  comment = ' Integer write '
  print 100, comment, t4
  write (10) y
  call cpu_time(t)
  t5 = t - t1 - t2 - t3 - t4
  comment = ' Real    write '
  print 100, comment, t5
100 format (1x, a, 2x, f7.3)
end program
