module factorial_module
  implicit none

contains
  recursive integer function factorial(i) result (answer)
    implicit none
    integer, intent (in) :: i

    if (i==0) then
      answer = 1
    else
      answer = i*factorial(i-1)
    end if
  end function
end module

program ch1208
  use factorial_module
  implicit none
  integer :: i, f

  print *, ' type in the number, integer only'
  read *, i
  do while (i<0)
    print *, ' factorial only defined for '
    print *, ' positive integers: re-input'
    read *, i
  end do
  f = factorial(i)
  print *, ' answer is', f
end program
