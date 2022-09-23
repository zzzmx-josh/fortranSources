module data_module
  implicit none
  integer, parameter :: n = 12
  real, dimension (1:n) :: rainfall
  real, dimension (1:n) :: sorted

contains
  subroutine readdata
    implicit none
    integer :: i
    character (len=40) :: filename

    print *, ' What is the filename ?'
    read *, filename
    open (unit=100, file=filename, status='old')
    do i = 1, n
      read (100, *) rainfall(i)
    end do
  end subroutine

  subroutine sortdata
    implicit none

    sorted = rainfall
    call selection
  contains
    subroutine selection
      implicit none
      integer :: i, j, k
      real :: minimum

      do i = 1, n - 1
        k = i
        minimum = sorted(i)
        do j = i + 1, n
          if (sorted(j)<minimum) then
            k = j
            minimum = sorted(k)
          end if
        end do
        sorted(k) = sorted(i)
        sorted(i) = minimum
      end do
    end subroutine
  end subroutine

  subroutine printdata
    implicit none
    integer :: i

    print *, ' original data is '
    do i = 1, n
      print 100, rainfall(i)
    end do
    print *, ' Sorted data is '
    do i = 1, n
      print 100, sorted(i)
    end do
100 format (1x, f7.1)
  end subroutine
end module

program ch2102
  use data_module
  implicit none

  call readdata
  call sortdata
  call printdata
end program
