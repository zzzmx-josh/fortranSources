module personal_module

  type address

    character (len=60) :: street
    character (len=60) :: district
    character (len=60) :: city
    character (len=8) :: post_code

  end type

  type date_of_birth

    integer :: day
    integer :: month
    integer :: year

  end type

  type personal
    character (len=20) :: first_name
    character (len=20) :: other_names
    character (len=40) :: surname
    type (date_of_birth) :: dob
    character (len=1) :: gender
    type (address) :: addr

  end type

end module

program ch1704

  use personal_module

  implicit none

  integer :: n_people
  integer :: i

  type (personal), dimension (:), allocatable :: p

  print *, 'input number of people'
  read *, n_people

  allocate (p(1:n_people))

  open (unit=1, file='person.txt', status='old')

  do i = 1, n_people

    read (1, fmt=100) p(i)%first_name, p(i)%other_names, p(i)%surname, p(i)%dob%day, p(i)%dob%month, p(i)%dob%year, p(i)%gender, &
      p(i)%addr%street, p(i)%addr%district, p(i)%addr%city, p(i)%addr%post_code

  end do

  do i = 1, n_people

    write (*, fmt=110) p(i)%first_name, p(i)%other_names, p(i)%surname, p(i)%dob%day, p(i)%dob%month, p(i)%dob%year, &
      p(i)%gender, p(i)%addr%street, p(i)%addr%district, p(i)%addr%city, p(i)%addr%post_code

  end do

100 format (a20, /, a20, /, a40, /, i2, 1x, i2, 1x, i4, /, a1, /, a60, /, a60, /, a60, /, a8)

110 format (a20, a20, a40, /, i2, 1x, i2, 1x, i4, /, a1, /, a60, /, a60, /, a60, /, a8)

end program
