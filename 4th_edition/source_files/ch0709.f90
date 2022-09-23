program ch0709
! variables used are
! mean - for the running mean
! ssq - the running corrected sum of squares
! x - input values for which
! mean and sd required
! w - local work variable
! sd - standard deviation
! r - another work variable
  implicit none
  real :: mean = 0.0, ssq = 0.0, x, w, sd, r
  integer :: i, n

  print *, ' enter the number of readings'
  read *, n
  print *, ' enter the ', n, ' values, one per line'
  do i = 1, n
    read *, x
    w = x - mean
    r = i - 1
    mean = (r*mean+x)/i
    ssq = ssq + w*w*r/i
  end do
  sd = (ssq/r)**0.5
  print *, ' mean is ', mean
  print *, ' standard deviation is ', sd
end program
