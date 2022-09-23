module read_module
  implicit none

contains
  subroutine readin(name, x, n)
    implicit none
    integer, intent (in) :: n
    real, dimension (:), intent (out) :: x
    character (len=*), intent (in) :: name
    integer :: i

    open (unit=10, file=name, status='old')
    do i = 1, n
      read (10, *) x(i)
    end do
    close (unit=10)
  end subroutine
end module

program ch2002
  use read_module
  implicit none
  real, allocatable, dimension (:) :: a
  integer :: nos, i
  character (len=20) :: filename

  print *, ' Type in the name of the data file'
  read '(a)', filename
  print *, ' Input the number of items'
  read *, nos
  allocate (a(1:nos))
  call readin(filename, a, nos)
  print *, ' data read in was'
  do i = 1, nos
    print *, ' ', a(i)
  end do
end program
