include 'ch3701_person_module.f90'

program ch3701

  use ch3701_person_module

  integer, parameter :: n = 4
  type (person), dimension (n) :: p
  integer :: i

  open (unit=99, file='ch3701_input_file.txt')

  do i = 1, n
    read (99, 100) p(i)
100 format (dt)
    write (*, 110) p(i)
110 format (dt)
  end do

end program
