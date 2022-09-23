module gcd_module
  implicit none

contains
  recursive integer function gcd(i, j) result (answer)
    implicit none
    integer, intent (in) :: i, j

    if (j==0) then
      answer = i
    else
      answer = gcd(j, mod(i,j))
    end if
  end function
end module

program ch1209
  use gcd_module
  implicit none
  integer :: i, j, result

  print *, ' type in two integers'
  read *, i, j
  result = gcd(i, j)
  print *, ' gcd is ', result
end program
