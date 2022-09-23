program ch41_dislin_05

  use dislin

  parameter (n=906)
  dimension x(n), y(n)

  open (unit=100, file='aberporth_rainfall.csv', status='old')

  do i = 1, n
    read (100, 100) x(i), y(i)
100 format (f3.0, 6x, f4.2)
  end do

! Must call initialisation routine

  call disini

! Plot a border round a page

  call pagera

! Bounding box
!
! 0 ,    0       2969 ,    0
! 0 , 2099       2969 , 2099

! Position of axis systems

  call axspos(450, 1800)

! axis length

  call axslen(2400, 1400)

! Change symbol rectangle by default

  call symbol(4, 0, 0)

! X axis

  call name('Months', 'X')

! Y axis

  call name('Rainfall inches', 'Y')

  call labdig(1, 'X')
  call ticks(1, 'XY')

  call titlin('Demonstration of scatterplot', 1)
  call titlin('of rainfall by month', 3)

! call mylab('Jan',1,'X')
! call mylab('Feb',2,'X')
! call mylab('Mar',3,'X')
! call mylab('Apr',4,'X')
! call mylab('May',5,'X')
! call mylab('Jun',6,'X')
! call mylab('Jul',7,'X')
! call mylab('Aug',8,'X')
! call mylab('Sep',9,'X')
! call mylab('Oct',10,'X')
! call mylab('Nov',11,'X')
! call mylab('Dec',12,'X')

! Plot a 2 d axis system

  call graf(0.0, 13.0, 1.0, 1.0, 0.0, 11.0, 0.0, 1.0)

! Scatter plot

  call qplsca(x, y, n)

! Must call terminating routine

  call disfin

end program
