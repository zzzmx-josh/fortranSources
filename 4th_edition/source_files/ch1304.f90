program ch1304
  implicit none

! Simple counting of vowels, consonants,
! digits, blanks and the rest

  integer :: vowels = 0, consonants = 0, digits = 0
  integer :: blank = 0, other = 0, i
  character :: letter
  character (len=80) :: line

  read '(a)', line
  do i = 1, 80
    letter = line(i:i)
!   the above extracts one character
!   at position i
    select case (letter)
    case ('A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u')
      vowels = vowels + 1
    case ('B', 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Y', 'Z', 'b', 'c', &
        'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'y', 'z')
      consonants = consonants + 1
    case ('1', '2', '3', '4', '5', '6', '7', '8', '9', '0')
      digits = digits + 1
    case (' ')
      blank = blank + 1
    case default
      other = other + 1
    end select
  end do
  print *, ' Vowels = ', vowels
  print *, ' Consonants = ', consonants
  print *, ' Digits = ', digits
  print *, ' Blanks = ', blank
  print *, ' Other characters = ', other
end program
