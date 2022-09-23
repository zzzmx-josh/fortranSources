module address_module

  type address

    character (len=40) :: name
    character (len=60) :: street
    character (len=60) :: district
    character (len=60) :: city
    character (len=8) :: post_code

  end type

end module

program ch1703

  use address_module

  implicit none

  integer :: n_of_address

  type (address), dimension (:), allocatable :: addr

  integer :: i

  print *, 'input number of addresses'
  read *, n_of_address

  allocate (addr(1:n_of_address))

  open (unit=1, file='address.txt', status='old')

  do i = 1, n_of_address

    read (unit=1, fmt='(a40)') addr(i)%name
    read (unit=1, fmt='(a60)') addr(i)%street
    read (unit=1, fmt='(a60)') addr(i)%district
    read (unit=1, fmt='(a60)') addr(i)%city
    read (unit=1, fmt='(a8)') addr(i)%post_code

  end do

  do i = 1, n_of_address

    print *, addr(i)%name
    print *, addr(i)%street
    print *, addr(i)%district
    print *, addr(i)%city
    print *, addr(i)%post_code

  end do

end program
