module t_position
  implicit none
  type position
    integer :: x
    integer :: y
    integer :: z
  end type
  interface operator (+)
    module procedure new_position
  end interface

contains
  function new_position(a, b)
    type (position), intent (in) :: a, b
    type (position) :: new_position

    new_position%x = a%x + b%x
    new_position%y = a%y + b%y
    new_position%z = a%z + b%z
  end function
end module

program ch2401
  use t_position
  implicit none
  type (position) :: a, b, c

  a%x = 10
  a%y = 10
  a%z = 10
  b%x = 20
  b%y = 20
  b%z = 20
  c = a + b
  print *, a
  print *, b
  print *, c
end program
