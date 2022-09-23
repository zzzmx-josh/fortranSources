program ch0707
  implicit none
  real, dimension (-180:180) :: time = 0
  integer :: degree, strip
  real :: value

  do degree = -180, 165, 15
    value = degree/15.
    do strip = 0, 14
      time(degree+strip) = value
    end do
  end do
  do degree = -180, 180
    print *, degree, ' ', time(degree)
  end do
end program
