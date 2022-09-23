program ch1211
  implicit none
  real :: result, n, r

  print *, ' type in n and r'
  read *, n, r
! number of possible combinations that can
! be formed when
! r objects are selected out of a group of n
! n!/r!(n-r)!
  result = stirling(n)/(stirling(r)*stirling(n-r))
  print *, result
  print *, n, r
contains
  real function stirling(x)
    real, intent (in) :: x
    real, parameter :: pi = 3.1415927, e = 2.7182828

    stirling = sqrt(2.*pi*x)*(x/e)**x
  end function
end program
