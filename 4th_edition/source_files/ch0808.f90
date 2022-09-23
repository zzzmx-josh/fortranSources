program ch0808
  implicit none
  integer, dimension (1:2, 1:4) :: x
  integer, dimension (1:8) :: y = (/ 1, 2, 3, 4, 5, 6, 7, 8 /)
  integer, dimension (1:6) :: z = (/ 1, 2, 3, 4, 5, 6 /)
  integer :: r, c

  print *, ' Source array y'
  print *, y
  print *, ' Source array z'
  print *, z
  print *, ' Simple reshape sizes match'
  x = reshape(y, (/2,4/) )
  do r = 1, 2
    print *, (x(r,c), c=1, 4)
  end do
  print *, ' Source 2 elements smaller pad with 0'
  x = reshape(z, (/2,4/), (/0,0/) )
  do r = 1, 2
    print *, (x(r,c), c=1, 4)
  end do
  print *, ' As previous now specify order as 1*2'
  x = reshape(z, (/2,4/), (/0,0/), (/1,2/) )
  do r = 1, 2
    print *, (x(r,c), c=1, 4)
  end do
  print *, ' As previous now specify order as 2*1'
  x = reshape(z, (/2,4/), (/0,0/), (/2,1/) )
  do r = 1, 2
    print *, (x(r,c), c=1, 4)
  end do
end program
