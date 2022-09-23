program ch1403
  implicit none
  character (80) :: string, strip
  integer :: ipos, i, length = 80

  ipos = 0
  print *, ' type in a string'
  read '(a)', string
  do i = 1, length
    if (string(i:i)/=' ') then
      ipos = ipos + 1
      strip(ipos:ipos) = string(i:i)
    end if
  end do
  print *, string
  print *, strip
end program
