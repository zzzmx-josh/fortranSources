subroutine dsort(dx, dy, n, kflag)
! ***BEGIN PROLOGUE  DSORT
! ***PURPOSE  Sort an array and optionally make the same interchanges in
! an auxiliary array.  The array may be sorted in increasing
! or decreasing order.  A slightly modified QUICKSORT
! algorithm is used.
! ***LIBRARY   SLATEC
! ***CATEGORY  N6A2B
! ***TYPE      DOUBLE PRECISION (SSORT-S, DSORT-D, ISORT-I)
! ***KEYWORDS  SINGLETON QUICKSORT, SORT, SORTING
! ***AUTHOR  Jones, R. E., (SNLA)
! Wisniewski, J. A., (SNLA)
! ***DESCRIPTION
!
! DSORT sorts array DX and optionally makes the same interchanges in
! array DY.  The array DX may be sorted in increasing order or
! decreasing order.  A slightly modified quicksort algorithm is used.
!
! Description of Parameters
! DX - array of values to be sorted   (usually abscissas)
! DY - array to be (optionally) carried along
! N  - number of values in array DX to be sorted
! KFLAG - control parameter
! =  2  means sort DX in increasing order and carry DY along.
! =  1  means sort DX in increasing order (ignoring DY)
! = -1  means sort DX in decreasing order (ignoring DY)
! = -2  means sort DX in decreasing order and carry DY along.
!
! ***REFERENCES  R. C. Singleton, Algorithm 347, An efficient algorithm
! for sorting with minimal storage, Communications of
! the ACM, 12, 3 (1969), pp. 185-187.
! ***ROUTINES CALLED  XERMSG
! ***REVISION HISTORY  (YYMMDD)
! 761101  DATE WRITTEN
! 761118  Modified to use the Singleton quicksort algorithm.  (JAW)
! 890531  Changed all specific intrinsics to generic.  (WRB)
! 890831  Modified array declarations.  (WRB)
! 891009  Removed unreferenced statement labels.  (WRB)
! 891024  Changed category.  (WRB)
! 891024  REVISION DATE from Version 3.2
! 891214  Prologue converted to Version 4.0 format.  (BAB)
! 900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
! 901012  Declared all variables; changed X,Y to DX,DY; changed
! code to parallel SSORT. (M. McClain)
! 920501  Reformatted the REFERENCES section.  (DWL, WRB)
! 920519  Clarified error messages.  (DWL)
! 920801  Declarations section rebuilt and code restructured to use
! IF-THEN-ELSE-ENDIF.  (RWC, WRB)
! ***END PROLOGUE  DSORT
! .. Scalar Arguments ..
  integer kflag, n
! .. Array Arguments ..
  double precision dx(*), dy(*)
! .. Local Scalars ..
  double precision r, t, tt, tty, ty
  integer i, ij, j, k, kk, l, m, nn
! .. Local Arrays ..
  integer il(21), iu(21)
! .. External Subroutines ..
! EXTERNAL XERMSG
! .. Intrinsic Functions ..
  intrinsic abs, int
! ***FIRST EXECUTABLE STATEMENT  DSORT
  nn = n
! IF (NN .LT. 1) THEN
! CALL XERMSG ('SLATEC', 'DSORT',
! +      'The number of values to be sorted is not positive.', 1, 1)
! RETURN
! ENDIF
!
  kk = abs(kflag)
! IF (KK.NE.1 .AND. KK.NE.2) THEN
! CALL XERMSG ('SLATEC', 'DSORT',
! +      'The sort control parameter, K, is not 2, 1, -1, or -2.', 2
! +      1)
! RETURN
! ENDIF
!
! Alter array DX to get decreasing order if needed
!
  if (kflag<=-1) then
    do i = 1, nn
      dx(i) = -dx(i)
    end do
  end if
!
  if (kk==2) go to 180
!
! Sort DX only
!
  m = 1
  i = 1
  j = nn
  r = 0.375d0
!
100 if (i==j) go to 140
  if (r<=0.5898437d0) then
    r = r + 3.90625d-2
  else
    r = r - 0.21875d0
  end if
!
110 k = i
!
! Select a central element of the array and save it in location T
!
  ij = i + int((j-i)*r)
  t = dx(ij)
!
! If first element of array is greater than T, interchange with T
!
  if (dx(i)>t) then
    dx(ij) = dx(i)
    dx(i) = t
    t = dx(ij)
  end if
  l = j
!
! If last element of array is less than than T, interchange with T
!
  if (dx(j)<t) then
    dx(ij) = dx(j)
    dx(j) = t
    t = dx(ij)
!
!   If first element of array is greater than T, interchange with T
!
    if (dx(i)>t) then
      dx(ij) = dx(i)
      dx(i) = t
      t = dx(ij)
    end if
  end if
!
! Find an element in the second half of the array which is smaller
! than T
!
120 l = l - 1
  if (dx(l)>t) go to 120
!
! Find an element in the first half of the array which is greater
! than T
!
130 k = k + 1
  if (dx(k)<t) go to 130
!
! Interchange these elements
!
  if (k<=l) then
    tt = dx(l)
    dx(l) = dx(k)
    dx(k) = tt
    go to 120
  end if
!
! Save upper and lower subscripts of the array yet to be sorted
!
  if (l-i>j-k) then
    il(m) = i
    iu(m) = l
    i = k
    m = m + 1
  else
    il(m) = k
    iu(m) = j
    j = l
    m = m + 1
  end if
  go to 150
!
! Begin again on another portion of the unsorted array
!
140 m = m - 1
  if (m==0) go to 270
  i = il(m)
  j = iu(m)
!
150 if (j-i>=1) go to 110
  if (i==1) go to 100
  i = i - 1
!
160 i = i + 1
  if (i==j) go to 140
  t = dx(i+1)
  if (dx(i)<=t) go to 160
  k = i
!
170 dx(k+1) = dx(k)
  k = k - 1
  if (t<dx(k)) go to 170
  dx(k+1) = t
  go to 160
!
! Sort DX and carry DY along
!
180 m = 1
  i = 1
  j = nn
  r = 0.375d0
!
190 if (i==j) go to 230
  if (r<=0.5898437d0) then
    r = r + 3.90625d-2
  else
    r = r - 0.21875d0
  end if
!
200 k = i
!
! Select a central element of the array and save it in location T
!
  ij = i + int((j-i)*r)
  t = dx(ij)
  ty = dy(ij)
!
! If first element of array is greater than T, interchange with T
!
  if (dx(i)>t) then
    dx(ij) = dx(i)
    dx(i) = t
    t = dx(ij)
    dy(ij) = dy(i)
    dy(i) = ty
    ty = dy(ij)
  end if
  l = j
!
! If last element of array is less than T, interchange with T
!
  if (dx(j)<t) then
    dx(ij) = dx(j)
    dx(j) = t
    t = dx(ij)
    dy(ij) = dy(j)
    dy(j) = ty
    ty = dy(ij)
!
!   If first element of array is greater than T, interchange with T
!
    if (dx(i)>t) then
      dx(ij) = dx(i)
      dx(i) = t
      t = dx(ij)
      dy(ij) = dy(i)
      dy(i) = ty
      ty = dy(ij)
    end if
  end if
!
! Find an element in the second half of the array which is smaller
! than T
!
210 l = l - 1
  if (dx(l)>t) go to 210
!
! Find an element in the first half of the array which is greater
! than T
!
220 k = k + 1
  if (dx(k)<t) go to 220
!
! Interchange these elements
!
  if (k<=l) then
    tt = dx(l)
    dx(l) = dx(k)
    dx(k) = tt
    tty = dy(l)
    dy(l) = dy(k)
    dy(k) = tty
    go to 210
  end if
!
! Save upper and lower subscripts of the array yet to be sorted
!
  if (l-i>j-k) then
    il(m) = i
    iu(m) = l
    i = k
    m = m + 1
  else
    il(m) = k
    iu(m) = j
    j = l
    m = m + 1
  end if
  go to 240
!
! Begin again on another portion of the unsorted array
!
230 m = m - 1
  if (m==0) go to 270
  i = il(m)
  j = iu(m)
!
240 if (j-i>=1) go to 200
  if (i==1) go to 190
  i = i - 1
!
250 i = i + 1
  if (i==j) go to 230
  t = dx(i+1)
  ty = dy(i+1)
  if (dx(i)<=t) go to 250
  k = i
!
260 dx(k+1) = dx(k)
  dy(k+1) = dy(k)
  k = k - 1
  if (t<dx(k)) go to 260
  dx(k+1) = t
  dy(k+1) = ty
  go to 250
!
! Clean up
!
270 if (kflag<=-1) then
    do i = 1, nn
      dx(i) = -dx(i)
    end do
  end if
  return
end subroutine

