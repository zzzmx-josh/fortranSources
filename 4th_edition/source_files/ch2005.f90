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

program ch2005
  use sort_data_module
  implicit none
  integer, parameter :: n = 10000000
  real, allocatable, dimension (:) :: x
  integer, dimension (8) :: timing
  real :: t1, t2
  character *30, dimension (4) :: heading = [ ' Allocate =                 ', ' Random number generation = ', &
    ' Sort =                     ', ' Deallocate =               ' ]

  call date_and_time(values=timing)
  print *, ' Program starts'
  write (unit=*, fmt=100) timing(1:3), timing(5:7)
100 format (2x, i4, 2('/',i2), ' ', 2(i2,':'), i2)
  t1 = td()
  allocate (x(n))
  t2 = td()
  write (unit=*, fmt=110) heading(1), (t2-t1)
110 format (a30, f8.3)
  t1 = t2
!
! Random number generation
  call random_number(x)
  t2 = td()
  write (unit=*, fmt=110) heading(2), (t2-t1)
  t1 = t2
!
! Sorting
  call sort_data(x, n)
  t2 = td()
  write (unit=*, fmt=110) heading(3), (t2-t1)
  print *, ' First 10 sorted numbers are'
  write (unit=*, fmt=120) x(1:10)
120 format (2x, e14.6)
  t1 = t2
!
! Deallocation
  deallocate (x)
  t2 = td()
  write (unit=*, fmt=110) heading(4), (t2-t1)
  call date_and_time(values=timing)
  print *, ' Program terminates'
  write (unit=*, fmt=100) timing(1:3), timing(5:7)

contains

  function td()
    real :: td

    call date_and_time(values=timing)
    td = 60*timing(6) + timing(7) + real(timing(8))/1000.0
  end function
end program
