subroutine dsort_qp(dx, dy, n, kflag)
  use precision_module, wp => qp
  implicit none
! .. Scalar Arguments ..
  integer kflag, n
! .. Array Arguments ..
  real (wp) :: dx(*), dy(*)
! .. Local Scalars ..
  real (wp) :: r, t, tt, tty, ty
  integer i, ij, j, k, kk, l, m, nn
! .. Local Arrays ..
  integer il(21), iu(21)
! .. Intrinsic Functions ..
  intrinsic abs, int
! ***FIRST EXECUTABLE STATEMENT  DSORT
  nn = n
  kk = abs(kflag)
!
! Alter array DX to get decreasing order if
! needed
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
  r = 0.375_wp
!
100 if (i==j) go to 140
  if (r<=0.5898437_wp) then
    r = r + 3.90625_wp/100.0_wp
  else
    r = r - 0.21875_wp
  end if
!
110 k = i
!
! Select a central element of the array and save
! it in location T
!
  ij = i + int((j-i)*r)
  t = dx(ij)
!
! If first element of array is greater than T,
! interchange with T
!
  if (dx(i)>t) then
    dx(ij) = dx(i)
    dx(i) = t
    t = dx(ij)
  end if
  l = j
!
! If last element of array is less than than T,
! interchange with T
!
  if (dx(j)<t) then
    dx(ij) = dx(j)
    dx(j) = t
    t = dx(ij)
!
!   If first element of array is greater than T,
!   interchange with T
!
    if (dx(i)>t) then
      dx(ij) = dx(i)
      dx(i) = t
      t = dx(ij)
    end if
  end if
!
! Find an element in the second half of the
! array which is smaller
! than T
!
120 l = l - 1
  if (dx(l)>t) go to 120
!
! Find an element in the first half of the array
! which is greater
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
! Save upper and lower subscripts of the array
! yet to be sorted
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
! Begin again on another portion of the unsorted
! array
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
  r = 0.375_wp
!
190 if (i==j) go to 230
  if (r<=0.5898437_wp) then
    r = r + 3.90625_wp/100.0_wp
  else
    r = r - 0.21875_wp
  end if
!
200 k = i
!
! Select a central element of the array and save
! it in location T
!
  ij = i + int((j-i)*r)
  t = dx(ij)
  ty = dy(ij)
!
! If first element of array is greater than T,
! interchange with T
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
! If last element of array is less than T,
! interchange with T
!
  if (dx(j)<t) then
    dx(ij) = dx(j)
    dx(j) = t
    t = dx(ij)
    dy(ij) = dy(j)
    dy(j) = ty
    ty = dy(ij)
!
!   If first element of array is greater than T,
!   interchange with T
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
! Find an element in the second half of the
! array which is smaller
! than T
!
210 l = l - 1
  if (dx(l)>t) go to 210
!
! Find an element in the first half of the array
! which is greater
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
! Save upper and lower subscripts of the array
! yet to be sorted
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
! Begin again on another portion of the unsorted
! array
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
