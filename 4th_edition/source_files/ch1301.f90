program ch1301
  implicit none
  real :: a, b, c, term, a2, root1, root2

! a b and c are the coefficients of the terms
! a*x**2+b*x+c
! find the roots of the quadratic, root1 and
! root2

  print *, ' give the coefficients a, b and c'
  read *, a, b, c
  term = b*b - 4.*a*c
  a2 = a*2.
! if term < 0, roots are complex
! if term = 0, roots are equal
! if term > 0, roots are real and different
  if (term<0.0) then
    print *, ' roots are complex'
  else if (term>0.0) then
    term = sqrt(term)
    root1 = (-b+term)/a2
    root2 = (-b-term)/a2
    print *, ' roots are ', root1, ' and ', root2
  else
    root1 = -b/a2
    print *, ' roots are equal, at ', root1
  end if
end program
