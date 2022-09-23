program ch0812
  implicit none
  real, dimension (-180:180) :: time = 0
  integer :: degree, strip
  real :: value
  character (len=1), dimension (-180:180) :: direction = ' '

  do degree = -180, 165, 15
    value = degree/15.
    do strip = 0, 14
      time(degree+strip) = value
    end do
  end do
  do degree = -180, 180
    print *, degree, ' ', time(degree)
  end do
  where (time>0.0)
    direction = 'E'
    elsewhere (time<0.0)
    direction = 'W'
  end where
  print *, direction
end program
