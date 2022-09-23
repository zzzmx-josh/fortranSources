program ch41_dislin_02
  use dislin
  implicit none
  integer :: i, j
! Total number of processors and hence data
! points
  integer, parameter :: nprocs = 64
! Number of percentage values from
! 10% -> 90% 9
! 95% 1
! Total 10
  integer, parameter :: nn = 10
  real, dimension (nn) :: pp = (/ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95 /)
  real, dimension (nprocs) :: x
  real, dimension (nprocs) :: y
  real, dimension (nprocs, nn) :: ydata
  integer :: nx
  integer :: ny
  character *30 cbuf

  do i = 1, nprocs
    x(i) = real(i)
  end do
! Amdahl calculations. Store in 2 d array and
! then
! assign to 1 d array for plotting.
  do i = 1, nprocs
    do j = 1, nn
      ydata(i, j) = 1/((1-pp(j))+pp(j)/i)
    end do
  end do
! Write the data to a file for verification
! purposes
  open (unit=10, file='amdahl_table_64.txt')
  do i = 1, nprocs
    write (unit=10, fmt=100) x(i), ydata(i, 1:nn)
100 format (11(f7.2,2x))
  end do
  close (10)
  call disini
  call complx
  call axspos(450, 1800)
  call axslen(2200, 1400)
  call name('Number of processors', 'x')
  call name('Speed up', 'y')
  call titlin('Plot of Amdahls Law', 1)
  call titlin('64 Processors', 3)
  call labdig(-1, 'x')
  call ticks(10, 'xy')
  call graf(1.0, 8.0, 1.0, 1.0, 1.0, 7.0, 1.0, 1.0)
  call title
  call xaxgit
  call chncrv('line')
! Plot the curves. Copy from 2 d array to 1 d
! array
! before the call to curve.
  do i = 1, nn
    y = ydata(1:nprocs, i)
    call curve(x, y, nprocs)
  end do
  call legini(cbuf, 10, 3)
! Coordinates of the start of the legend
! for the curves.
  nx = 500
  ny = 450
  call legpos(nx, ny)
  call leglin(cbuf, '10%', 1)
  call leglin(cbuf, '20%', 2)
  call leglin(cbuf, '30%', 3)
  call leglin(cbuf, '40%', 4)
  call leglin(cbuf, '50%', 5)
  call leglin(cbuf, '60%', 6)
  call leglin(cbuf, '70%', 7)
  call leglin(cbuf, '80%', 8)
  call leglin(cbuf, '90%', 9)
  call leglin(cbuf, '95%', 10)
  call legtit('legend')
  call legend(cbuf, 3)
  call disfin
end program
