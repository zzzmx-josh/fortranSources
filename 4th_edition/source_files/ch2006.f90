module read_data_module
  implicit none

contains
  subroutine read_data(file_name, raw_data, how_many)
    implicit none
    character (len=*), intent (in) :: file_name
    integer, intent (in) :: how_many
    real, intent (out), allocatable, dimension (:) :: raw_data
!   local variables
    integer :: i

    allocate (raw_data(1:how_many))
    open (unit=1, file=file_name, status='old')
    do i = 1, how_many
      read (unit=1, fmt=*) raw_data(i)
    end do
  end subroutine
end module

module sort_data_module
  implicit none

contains
  subroutine sort_data(raw_data, how_many)
    implicit none
    integer, intent (in) :: how_many
    real, intent (inout), dimension (:) :: raw_data

    call quicksort(1, how_many)
  contains
    recursive subroutine quicksort(l, r)
      implicit none
      integer, intent (in) :: l, r
!     local variables
      integer :: i, j
      real :: v, t

      i = l
      j = r
      v = raw_data(int((l+r)/2))
      do
        do while (raw_data(i)<v)
          i = i + 1
        end do
        do while (v<raw_data(j))
          j = j - 1
        end do
        if (i<=j) then
          t = raw_data(i)
          raw_data(i) = raw_data(j)
          raw_data(j) = t
          i = i + 1
          j = j - 1
        end if
        if (i>j) exit
      end do
      if (l<j) then
        call quicksort(l, j)
      end if
      if (i<r) then
        call quicksort(i, r)
      end if
    end subroutine
  end subroutine
end module

module print_data_module
  implicit none

contains
  subroutine print_data(raw_data, how_many)
    implicit none
    integer, intent (in) :: how_many
    real, intent (in), dimension (:) :: raw_data
!   local variables
    integer :: i

    open (file='sorted.txt', unit=2)
    do i = 1, how_many
      write (unit=2, fmt=*) raw_data(i)
    end do
    close (2)
  end subroutine
end module

program ch2006
  use read_data_module
  use sort_data_module
  use print_data_module
  implicit none
  integer :: how_many
  character (len=20) :: file_name
  real, allocatable, dimension (:) :: raw_data
  integer, dimension (8) :: timing

  print *, ' how many data items are there?'
  read *, how_many
  print *, ' what is the file name?'
  read '(a)', file_name
  call date_and_time(values=timing)
  print *, ' initial'
  print *, timing(6), timing(7), timing(8)
  call read_data(file_name, raw_data, how_many)
  call date_and_time(values=timing)
  print *, ' allocate and read'
  print *, timing(6), timing(7), timing(8)
  call sort_data(raw_data, how_many)
  call date_and_time(values=timing)
  print *, ' sort'
  print *, timing(6), timing(7), timing(8)
  call print_data(raw_data, how_many)
  call date_and_time(values=timing)
  print *, ' print'
  print *, timing(6), timing(7), timing(8)
  print *, ' '
  print *, ' data written to file sorted.txt'
end program
