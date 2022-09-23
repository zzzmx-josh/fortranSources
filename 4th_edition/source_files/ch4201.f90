module abstract_function_interface_module

  abstract interface
    real function f(i)
      implicit none
      integer, intent (in) :: i
    end function
  end interface

end module

module fun01

  implicit none

contains

  real function f1(i)

    implicit none
    integer, intent (in) :: i

    f1 = 1.0/i

  end function

  real function f2(i)

    implicit none
    integer, intent (in) :: i

    f2 = 1.0/(i*i)

  end function

end module

module fun02

  use abstract_function_interface_module

contains

  real function f3(fun, i)

    implicit none
    integer, intent (in) :: i

    procedure (f) :: fun

    f3 = fun(i)

  end function

  real function f4(fun, i)

    implicit none
    integer, intent (in) :: i

    procedure (f) :: fun

    integer :: n
    real :: t

    t = 0.0

    do n = 1, 5
      t = t + fun(i)
    end do

    f4 = t

  end function

end module

program ch4201

  use abstract_function_interface_module

  use fun01

  use fun02

  implicit none

  procedure (f), pointer :: p1

  p1 => f1

  print *, ' p1 => f1, calling f3'
  print *, f3(p1, 2)

  p1 => f2

  print *, ' p1 => f2, calling f3'
  print *, f3(p1, 2)

  p1 => f1

  print *, ' p1 => f1, calling f4'
  print *, f4(p1, 2)

  p1 => f2

  print *, ' p1 => f2, calling f4'
  print *, f4(p1, 2)

end program
