include 'timing_module.f90'

module character_binary_search_module

contains

  function binary_search(x, n, key) result (position)
    implicit none

!   Algorithm taken from Algorithms +
!   Data Structures - N. Wirth
!   ISBN 0-13-021999-1
!   Pages 57:59
    integer, intent (in) :: n
    character *32, dimension (1:n), intent (in) :: x
    character *32, intent (in) :: key

    integer :: position
    integer :: l, r, m

    l = 1
    r = n

    do while (l<r)
      m = (l+r)/2
      if (x(m)<key) then
        l = m + 1
      else
        r = m
      end if
    end do

    if (x(r)==key) then
      position = r
    else
      position = 0
    end if

  end function

end module

program ch3805
  use character_binary_search_module
  use timing_module
  implicit none

  integer, parameter :: nwords = 173528
  character *32, dimension (1:nwords) :: dictionary
  character *32 :: word
  character *1 :: answer
  integer :: position

  call start_timing()

  call read_words()

  write (*, 100) time_difference()
100 format (2x, f7.3)

  do

    print *, 'Type in the word you are looking for'
    read *, word

    write (*, 100) time_difference()

    position = binary_search(dictionary, nwords, word)

    write (*, 100) time_difference()

    if (position==0) then
      print *, ' Word not found'
    else
      write (*, 110) trim(word), position
110   format (a, ' found at position ', i6)
    end if

    print *, ' Try again (y/n) ?'
    read *, answer

    if ((answer=='y') .or. (answer=='Y')) then
      cycle
    else
      exit
    end if

  end do

  call end_timing()

contains

  subroutine read_words()
    implicit none
    integer :: i
    character *80 :: file_name = 'words.txt'

    open (unit=10, file=file_name, status='old')
    do i = 1, nwords
      read (10, 100) dictionary(i)
100   format (a)
    end do
    close (10)

  end subroutine

end program ch3805
