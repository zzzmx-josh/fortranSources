program ch1008
  implicit none
  integer, parameter :: n = 10000000
  integer, dimension (1:n) :: x
  real, dimension (1:n) :: y
  integer :: i
  real :: t, t1, t2, t3
  character *15 :: comment

  call cpu_time(t)
  t1 = t
  comment = ' Program starts '
  print 120, comment, t1
  open (unit=10, file='ch0913.txt', status='old')
  do i = 1, n
    read (10, 100) x(i)
  end do
  call cpu_time(t)
  t2 = t - t1
  comment = ' Integer read '
  print 120, comment, t2
  do i = 1, n
    read (10, 110) y(i)
  end do
  call cpu_time(t)
  t3 = t - t1 - t2
  comment = ' Real read '
  print 120, comment, t3
  do i = 1, 10
    print 130, x(i), y(i)
  end do
100 format (1x, i10)
110 format (1x, f10.0)
120 format (1x, a, 2x, f7.3)
130 format (1x, i4, 2x, f10.7)

end program
