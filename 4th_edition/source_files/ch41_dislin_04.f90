program ch41_dislin_04
  use dislin
  logical :: trial, screen
  real :: long, lat

  screen = .false.
  trial = .false.

! read in the tsunami data

  call datain(trial)

! I now have all the tsunami data latitude and
! longitude values read in to the arrays in the
! tsunam common block.

  iproj = 1
  lat = 0.0
  long = 180.0

  nreg = 0

! dislin initialisation routines and setting of
! some basic components
! of the plot. these are based on two sample
! dislin programs.

! initialise dislin

  call disini

! choose font

  call psfont('times-roman')

! determines the position of an axis system.
! the lower left corner of the axis system

  call axspos(400, 1850)

! the size of the axis system
! are the length and height of an axis system in
! plot coordinates. the default
! values are set to 2/3 of the page length and
! height.

  call axslen(2400, 1400)

! define axis title

  call name('longitude', 'x')

! define axis title

  call name('latitude', 'y')

! this routine plots a title over an axis
! system.

  call titlin('plot of 3034 tsunami events ', 3)

! determines which label types will be plotted
! on an axis.
! map defines geographical labels which are
! plotted as non negative floating-point
! numbers with the following characters 'w',
! 'e', 'n' and 's'.

  call labels('map', 'xy')

! plots a geographical axis system.

  call grafmp(-180., 180., -180., 90., -90., 90., -90., 30.)

! the statement call gridmp (i, j) overlays an
! axis system with a longitude
! and latitude grid where i and j are the number
! of grid lines between  labels in
! the x- and y-direction.

  call gridmp(1, 1)

! the routine world plots coastlines and lakes.

  call world

! the angle and height of the characters can be
! changed with the routines
! angle and height.

  call height(50)

! this routine plots a title over an axis
! system.
! the title may contain up to four lines of text
! designated
! with titlin.

  call title

! this is a call to the routine that actually
! plots each event.

  call plotem(trial, nreg)

! disfin terminates dislin and prints a message
! on the screen.
! the level is set back to 0.

  call disfin

end program

subroutine datain(trial)
  common /tsunam/reg0la(378), reg0lo(378), reg1la(206), reg1lo(206), reg2la(41), reg2lo(41), reg3la(54), reg3lo(54), reg4la(60), &
    reg4lo(60), reg5la(1540), reg5lo(1540), reg6la(80), reg6lo(80), reg7la(144), reg7lo(144), reg8la(245), reg8lo(245), &
    reg9la(285), reg9lo(285)

  logical :: trial
  character (80) :: filnam

  if (trial) then
    print *, ' entering data input phase'
  end if
  filnam = 'tsunami.txt'
  open (unit=50, file=filnam, err=100, status='old')
  go to 110
100 print *, ' error opening data file'
  print *, ' program terminates'
  stop
110 do i = 1, 378
    read (unit=50, fmt=120) reg0la(i), reg0lo(i)
  end do
120 format (1x, f7.2, 2x, f7.2)
  do i = 1, 206
    read (unit=50, fmt=120) reg1la(i), reg1lo(i)
  end do
  do i = 1, 41
    read (unit=50, fmt=120) reg2la(i), reg2lo(i)
  end do
  do i = 1, 54
    read (unit=50, fmt=120) reg3la(i), reg3lo(i)
  end do
  do i = 1, 60
    read (unit=50, fmt=120) reg4la(i), reg4lo(i)
  end do
  do i = 1, 1540
    read (unit=50, fmt=120) reg5la(i), reg5lo(i)
  end do
  do i = 1, 80
    read (unit=50, fmt=120) reg6la(i), reg6lo(i)
  end do
  do i = 1, 144
    read (unit=50, fmt=120) reg7la(i), reg7lo(i)
  end do
  do i = 1, 245
    read (unit=50, fmt=120) reg8la(i), reg8lo(i)
  end do
  do i = 1, 285
    read (unit=50, fmt=120) reg9la(i), reg9lo(i)
  end do
  if (trial) then
    do i = 1, 10
      print *, reg0la(i), '   ', reg0lo(i)
    end do
    print *, ' exiting data input phase'
    read *, dummy
  end if
end subroutine

subroutine plotem(trial, nreg)
  use dislin
  common /tsunam/reg0la(378), reg0lo(378), reg1la(206), reg1lo(206), reg2la(41), reg2lo(41), reg3la(54), reg3lo(54), reg4la(60), &
    reg4lo(60), reg5la(1540), reg5lo(1540), reg6la(80), reg6lo(80), reg7la(144), reg7lo(144), reg8la(245), reg8lo(245), &
    reg9la(285), reg9lo(285)

! this subroutine plots all of the tsunamis onto
! the map as coloured
! points, with a different colour per region. i
! have chosen
! a dot size of 1 mm, and step through the
! colour pallette.
! the default may not be appropriate.

  logical :: trial
  integer :: nreg
  integer :: kolour = 10
  data dwidth/1.0/

  if (trial) then
    dwidth = 5.0
    print *, ' entering plot points'
  end if
  call incmrk(-1)
  if (nreg==0) then
    call setclr(kolour)
    call curvmp(reg0lo, reg0la, 378)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg1lo, reg1la, 206)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg2lo, reg2la, 41)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg3lo, reg3la, 54)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg4lo, reg4la, 60)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg5lo, reg5la, 1540)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg6lo, reg6la, 80)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg7lo, reg7la, 144)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg8lo, reg8la, 245)
    kolour = kolour + 30
    call setclr(kolour)
    call curvmp(reg9lo, reg9la, 285)
  else if (nreg==1) then
    kolour = 10
    call setclr(kolour)
    call curvmp(reg0lo, reg0la, 378)
  else if (nreg==2) then
    kolour = 20
    call setclr(kolour)
    call curvmp(reg1lo, reg1la, 206)
  else if (nreg==3) then
    kolour = 30
    call setclr(kolour)
    call curvmp(reg2lo, reg2la, 41)
  else if (nreg==4) then
    kolour = 40
    call setclr(kolour)
    call curvmp(reg3lo, reg3la, 54)
  else if (nreg==5) then
    kolour = 50
    call setclr(kolour)
    call curvmp(reg4lo, reg4la, 60)
  else if (nreg==6) then
    kolour = 60
    call setclr(kolour)
    call curvmp(reg5lo, reg5la, 1540)
  else if (nreg==7) then
    kolour = 70
    call setclr(kolour)
    call curvmp(reg6lo, reg6la, 80)
  else if (nreg==8) then
    kolour = 80
    call setclr(kolour)
    call curvmp(reg7lo, reg7la, 144)
  else if (nreg==9) then
    kolour = 90
    call setclr(kolour)
    call curvmp(reg8lo, reg8la, 245)
  else if (nreg==10) then
    kolour = 100
    call setclr(kolour)
    call curvmp(reg9lo, reg9la, 285)
  end if
  if (trial) then
    print *, ' exiting plot points'
  end if

end subroutine
