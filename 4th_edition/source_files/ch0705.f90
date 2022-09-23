program ch0705
  implicit none
  integer, parameter :: nr = 5
  integer, parameter :: nc = 10
  integer, parameter :: nf = 3
  integer :: row, column, floor
  character *1, dimension (1:nr, 1:nc, 1:nf) :: seats = ' '

  do floor = 1, nf
    do row = 1, nr
      read *, (seats(row,column,floor), column=1, nc)
    end do
  end do
  print *, ' Seat plan is'
  do floor = 1, nf
    print *, ' Floor = ', floor
    do row = 1, nr
      print *, (seats(row,column,floor), column=1, nc)
    end do
  end do
end program
