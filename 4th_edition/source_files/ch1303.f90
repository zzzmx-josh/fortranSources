program ch1303
  implicit none
! Simple case statement example
  integer :: i, j, k
  character :: operator

  do
    print *, ' type in two integers'
    read *, i, j
    print *, ' type in operator'
    read '(a)', operator

calculator: select case (operator)
    case ('+') calculator
      k = i + j
      print *, ' Sum of numbers is ', k
    case ('-') calculator
      k = i - j
      print *, ' Difference is ', k
    case ('/') calculator
      k = i/j
      print *, ' Division is ', k
    case ('*') calculator
      k = i*j
      print *, ' Multiplication is ', k
    case default calculator
      exit
    end select calculator

  end do
end program
