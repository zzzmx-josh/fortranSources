module statistics_module
  implicit none

contains
  subroutine stats(x, n, mean, std_dev)
    implicit none
    integer, intent (in) :: n
    real, intent (in), dimension (:) :: x
    real, intent (out) :: mean
    real, intent (out) :: std_dev
    real :: variance
    real :: sumxi, sumxi2
    integer :: i

    variance = 0.0
    sumxi = 0.0
    sumxi2 = 0.0
    do i = 1, n
      sumxi = sumxi + x(i)
      sumxi2 = sumxi2 + x(i)*x(i)
    end do
    mean = sumxi/n
    variance = (sumxi2-sumxi*sumxi/n)/(n-1)
    std_dev = sqrt(variance)
  end subroutine
end module

program ch2001
  use statistics_module
  implicit none
  integer, parameter :: n = 10
  real, dimension (1:n) :: x
  real, dimension (-4:5) :: y
  real, dimension (10) :: z
  real, allocatable, dimension (:) :: t
  real :: m, sd
  integer :: i

  do i = 1, n
    x(i) = real(i)
  end do
  call stats(x, n, m, sd)
  print *, ' x'
  print 100, m, sd
100 format (' Mean = ', f7.3, ' Std Dev = ', f7.3)
  y = x
  call stats(y, n, m, sd)
  print *, ' y'
  print 100, m, sd
  z = x
  call stats(z, 10, m, sd)
  print *, ' z'
  print 100, m, sd
  allocate (t(n))
  t = x
  call stats(t, 10, m, sd)
  print *, ' t'
  print 100, m, sd
end program
