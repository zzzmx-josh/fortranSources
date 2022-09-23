program ch1407
  implicit none
  character (1024) :: string01
  character (1) :: set = ' '
  integer :: i
  integer :: l
  integer :: start, end

  string01 = 'The important issue about a language, is not so'
  string01 = trim(string01) // ' ' // 'much what features the language possesses, but'
  string01 = trim(string01) // ' ' // 'the features it does possess, are sufficient, to'
  string01 = trim(string01) // ' ' // 'support the desired programming styles, in the'
  string01 = trim(string01) // ' ' // 'desired application areas.'
  l = len(trim(string01))
  print *, ' Length of string is = ', l
  print *, ' String is'
  print *, trim(string01)
  start = 1
  end = l
  print *, ' Blanks at positions '
  do
    i = scan(string01(start:end), set)
    start = start + i
    if (i==0) exit
    write (*, 100, advance='no') start - 1
  end do
100 format (i5)
end program
