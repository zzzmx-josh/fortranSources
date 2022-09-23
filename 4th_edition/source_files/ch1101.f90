program ch1101
  implicit none
  integer :: filestat
  real :: x
  character (len=20) :: which

  do

    write (unit=6, fmt='("data file name,or end")')
    read (unit=5, fmt='(a)') which
    if (which=='end') exit
    open (unit=1, file=which, iostat=filestat, status='old')
    if (filestat>0) then
      print *, 'error opening file, please check'
      stop
    end if
    read (unit=1, fmt=100) x
    write (unit=6, fmt=110) which, x
    close (unit=1)

  end do

100 format (f6.0)
110 format ('from file ', a, ' x = ', f8.2)
end program
