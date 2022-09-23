module swap_module
  implicit none

contains
  elemental subroutine swap(x, y)
    integer, intent (inout) :: x, y
    integer :: temp

    temp = x
    x = y
    y = temp
  end subroutine
end module

program ch2007
  use swap_module
  implicit none
  integer, dimension (10) :: a, b
  integer :: i

  do i = 1, 10
    a(i) = i
    b(i) = i*i
  end do
  print *, a
  print *, b
  call swap(a, b)
  print *, a
  print *, b
end program
