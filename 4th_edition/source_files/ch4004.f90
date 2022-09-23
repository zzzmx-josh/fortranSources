module date_sub

! COLLECTED AND PUT TOGETHER JANUARY 1972, H. D.
! KNOBLE .
! ORIGINAL REFERENCES ARE CITED IN EACH ROUTINE.

! Code converted using TO_F90 by Alan Miller
! Date: 1999-12-22  Time: 10:23:47
! Compatible with Imagine1 F compiler:
! 2002-07-19

  implicit none

  public :: iday, izlr, calend, cdate, ndays, daysub, jd

contains

! ARITHMETIC FUNCTIONS "IZLR" AND "IDAY" ARE
! TAKEN FROM REMARK ON
! ALGORITHM 398, BY J. DOUGLAS ROBERTSON, CACM
! 15(10):918.

  function iday(yyyy, mm, dd) result (ival)
!   IDAY IS A COMPANION TO CALEND; GIVEN A
!   CALENDAR DATE, YYYY, MM,
!   DD, IDAY IS RETURNED AS THE DAY OF THE YEAR.
!   EXAMPLE: IDAY(1984, 4, 22) = 113

    integer, intent (in) :: yyyy, mm, dd
    integer :: ival

    ival = 3055*(mm+2)/100 - (mm+10)/13*2 - 91 + (1-(modulo(yyyy,4)+3)/4+(modulo(yyyy,100)+99)/100-(modulo(yyyy, &
      400)+399)/400)*(mm+10)/13 + dd

    return
  end function


  function izlr(yyyy, mm, dd) result (ival)
!   IZLR(YYYY, MM, DD) GIVES THE WEEKDAY NUMBER
!   0 = SUNDAY, 1 = MONDAY,
!   ... 6 = SATURDAY.  EXAMPLE: IZLR(1970, 1, 1)
!   = 4 = THURSDAY

    integer, intent (in) :: yyyy, mm, dd
    integer :: ival

    ival = modulo((13*(mm+10-(mm+10)/13*12)-1)/5+dd+77+5*(yyyy+(mm-14)/12-(yyyy+(mm-14)/12)/100*100)/4+(yyyy+(mm-14)/12)/400-( &
      yyyy+(mm-14)/12)/100*2, 7)

    return
  end function


  subroutine calend(yyyy, ddd, mm, dd)
!   CALEND WHEN GIVEN A VALID YEAR, YYYY, AND
!   DAY OF THE YEAR, DDD,
!   RETURNS THE MONTH, MM, AND DAY OF THE MONTH,
!   DD.
!   SEE ACM ALGORITHM 398, TABLELESS DATE
!   CONVERSION, BY
!   DICK STONE, CACM 13(10):621.

    integer, intent (in) :: yyyy
    integer, intent (in) :: ddd
    integer, intent (out) :: mm
    integer, intent (out) :: dd

    integer :: t

    t = 0
    if (modulo(yyyy,4)==0) t = 1

!   ------THE FOLLOWING STATEMENT IS NECESSARY
!   IF YYYY IS < 1900 OR > 2100.
    if (modulo(yyyy,400)/=0 .and. modulo(yyyy,100)==0) t = 0

    dd = ddd
    if (ddd>59+t) dd = dd + 2 - t
    mm = ((dd+91)*100)/3055
    dd = (dd+91) - (mm*3055)/100
    mm = mm - 2
!   ----------MM WILL BE CORRECT IFF DDD IS
!   CORRECT FOR YYYY.
    if (mm>=1 .and. mm<=12) return
    write (unit=*, fmt='(a,i11,a)') '$$CALEND: DAY OF THE YEAR INPUT =', ddd, ' IS OUT OF RANGE.'
    stop
  end subroutine


  subroutine cdate(jd, yyyy, mm, dd)
!   GIVEN A JULIAN DAY NUMBER, NNNNNNNN,
!   YYYY,MM,DD ARE RETURNED AS THE
!   CALENDAR DATE. JD = NNNNNNNN IS THE JULIAN
!   DATE FROM AN EPOCH
!   IN THE VERY DISTANT PAST.  SEE CACM 1968
!   11(10):657,
!   LETTER TO THE EDITOR BY FLIEGEL AND VAN
!   FLANDERN.
!   EXAMPLE CALL CDATE(2440588, YYYY, MM, DD)
!   RETURNS 1970 1 1 .

    integer, intent (in) :: jd
    integer, intent (out) :: yyyy
    integer, intent (out) :: mm
    integer, intent (out) :: dd

    integer :: l, n

    l = jd + 68569
    n = 4*l/146097
    l = l - (146097*n+3)/4
    yyyy = 4000*(l+1)/1461001
    l = l - 1461*yyyy/4 + 31
    mm = 80*l/2447
    dd = l - 2447*mm/80
    l = mm/11
    mm = mm + 2 - 12*l
    yyyy = 100*(n-49) + yyyy + l
    return
  end subroutine


  subroutine daysub(jd, yyyy, mm, dd, wd, ddd)
!   GIVEN JD, A JULIAN DAY # (SEE ASF JD), THIS
!   ROUTINE CALCULATES DD,
!   THE DAY NUMBER OF THE MONTH; MM, THE MONTH
!   NUMBER; YYYY THE YEAR;
!   WD THE WEEKDAY NUMBER, AND DDD THE DAY
!   NUMBER OF THE YEAR.

!   EXAMPLE:
!   CALL DAYSUB(2440588, YYYY, MM, DD, WD, DDD)
!   YIELDS 1970 1 1 4 1.

    integer, intent (in) :: jd
    integer, intent (out) :: yyyy
    integer, intent (out) :: mm
    integer, intent (out) :: dd
    integer, intent (out) :: wd
    integer, intent (out) :: ddd

    call cdate(jd, yyyy, mm, dd)
    wd = izlr(yyyy, mm, dd)
    ddd = iday(yyyy, mm, dd)

    return
  end subroutine


  function jd(yyyy, mm, dd) result (ival)

    integer, intent (in) :: yyyy
    integer, intent (in) :: mm
    integer, intent (in) :: dd
    integer :: ival

!   DATE ROUTINE JD(YYYY, MM, DD) CONVERTS
!   CALENDER DATE TO
!   JULIAN DATE.  SEE CACM 1968 11(10):657,
!   LETTER TO THE
!   EDITOR BY HENRY F. FLIEGEL AND THOMAS C. VAN
!   FLANDERN.
!   EXAMPLE JD(1970, 1, 1) = 2440588

    ival = dd - 32075 + 1461*(yyyy+4800+(mm-14)/12)/4 + 367*(mm-2-((mm-14)/12)*12)/12 - 3*((yyyy+4900+(mm-14)/12)/100)/4

    return
  end function


  function ndays(mm1, dd1, yyyy1, mm2, dd2, yyyy2) result (ival)

    integer, intent (in) :: mm1
    integer, intent (in) :: dd1
    integer, intent (in) :: yyyy1
    integer, intent (in) :: mm2
    integer, intent (in) :: dd2
    integer, intent (in) :: yyyy2
    integer :: ival

!   NDAYS IS RETURNED AS THE NUMBER OF DAYS
!   BETWEEN TWO
!   DATES; THAT IS  MM1/DD1/YYYY1 MINUS
!   MM2/DD2/YYYY2,
!   WHERE DATEI AND DATEJ HAVE ELEMENTS MM, DD,
!   YYYY.
!   NDAYS WILL BE POSITIVE IFF DATE1 IS MORE
!   RECENT THAN DATE2.

    ival = jd(yyyy1, mm1, dd1) - jd(yyyy2, mm2, dd2)

    return
  end function

end module


program test_datesub

! ======DATESUB.FOR with Sample Drivers.

  use date_sub
  implicit none
  integer :: yyyy, mm, dd, wd, ddd, mma, dda, ndiff, i
  integer, dimension (8) :: val

! Compute date this year for changing clocks
! back to EST.
! I.e.compute date for the last Sunday in
! October for this year.
  call date_and_time(values=val)
  yyyy = val(1)
  do i = 31, 26, -1
    if (izlr(yyyy,10,i)==0) then
      print *, 'Turn Clocks back to EST on: ', i, ' October ', yyyy
      exit
    end if
  end do
! Compute date this year for turning clocks
! ahead to DST
! I.e., compute date for the first Sunday in
! April for this year.
  call date_and_time(values=val)
  yyyy = val(1)
  do i = 1, 8
    if (izlr(yyyy,4,i)==0) then
      print *, 'Turn Clocks ahead to DST on: ', i, ' April ', yyyy
      exit
    end if
  end do

  call date_and_time(values=val)
  yyyy = val(1)

! Is this a leap year? I.e. is 12/31/yyyy the
! 366th day of the year?
  if (iday(yyyy,12,31)==366) then
    print *, yyyy, ' is a Leap Year'
  else
    print *, yyyy, ' is not a Leap Year'
  end if

! DAYSUB SHOULD RETURN: 1970, 1, 1, 4, 1
  call daysub(jd(1970,1,1), yyyy, mm, dd, wd, ddd)
  if (yyyy/=1970 .or. mm/=1 .or. dd/=1 .or. wd/=4 .or. ddd/=1) then
    print *, 'DAYSUB Failed; YYYY, MM, DD, WD, DDD = ', yyyy, mm, dd, wd, ddd
    stop
  end if

! DIFFERENCE BETWEEN TO SAME MONTHS AND DAYS
! OVER 1 LEAP YEAR IS 366.
  ndiff = ndays(5, 22, 1984, 5, 22, 1983)
  if (ndiff/=366) then
    print *, 'NDAYS FAILED; NDIFF = ', ndiff
  else
!   RECOVER MONTH AND DAY FROM YEAR AND DAY
!   NUMBER.
    call calend(yyyy, ddd, mma, dda)
    if (mma/=1 .and. dda/=1) then
      print *, 'CALEND FAILED; MMA, DDA = ', mma, dda
    else
      print *, '** DATE MANIPULATION SUBROUTINES SIMPLE TEST OK.'
    end if
  end if

  stop
end program
