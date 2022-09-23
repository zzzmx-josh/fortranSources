module gcd_module
  implicit none

contains
  integer function gcd(i, j)
    implicit none
    integer, intent (inout) :: i, j
    integer :: temp

    do while (j/=0)
      temp = mod(i, j)
      i = j
      j = temp
    end do
    gcd = i
  end function
end module

program ch1210
  use gcd_module
  implicit none
  integer :: i, j, result

  print *, ' type in two integers'
  read *, i, j
  result = gcd(i, j)
  print *, ' gcd is ', result
end program
