include 'precision_module.f90'

program ch2301

  use precision_module, wp => dp
  implicit none

  integer, parameter :: nn = 10
  integer :: n
  integer, dimension (nn) :: nvalues = [ 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 ]
  integer :: i
  character *80 heading

  heading = ' i           n   O(1)       O(n)'
  heading = trim(heading) // '      O(n*n) O(log n)  O(n log n)'
  print *, heading
  print *, ' '
  do i = 1, nn
    n = nvalues(i)
    print 100, i, n, order_1(), order_n(n), order_n_squared(n), order_log_n(n), order_n_log_n(n)
100 format (1x, i2, 2x, i10, 2x, i4, 2x, i10, 2x, e12.4, 2x, f7.2, 2x, e12.4)
  end do

contains

  integer function order_1()

    order_1 = 1
  end function

  integer function order_n(n)
    integer, intent (in) :: n

    order_n = n
  end function

  function order_n_squared(n)
    use precision_module, wp => dp
    integer, intent (in) :: n
    real (wp) :: order_n_squared

    order_n_squared = dble(n)*dble(n)
  end function

  real function order_log_n(n)
    integer, intent (in) :: n

    order_log_n = log(real(n))
  end function

  real function order_n_log_n(n)
    integer, intent (in) :: n

    order_n_log_n = n*log(real(n))
  end function

end program
