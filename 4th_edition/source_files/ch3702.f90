include 'ch3702_person_module.f90'

program ch3702

  use ch3702_person_module
  integer, parameter :: n = 4
  type (person), dimension (n) :: p
  integer :: i
  integer :: file_stat = 0

  open (unit=99, file='ch3701_input_file.txt', status='old', iostat=file_stat)

  if (file_stat/=0) then
    print *, ' File not found'
    print *, ' Program terminates'
    stop
  end if

  do i = 1, n
    read (99, 100) p(i)
100 format (dt(30, 3, 4, 2, 3))
    write (*, 110) p(i)
110 format (dt(20, 5, 4, 2, 3))
  end do

end program
