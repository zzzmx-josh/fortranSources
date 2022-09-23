subroutine isort_64(ix, iy, n, kflag)
  use integer_kind_module, wp => i64
  implicit none
! .. Scalar Arguments ..
  integer (wp) :: kflag, n
! .. Array Arguments ..
  integer (wp) :: ix(*), iy(*)
! .. Local Scalars ..
  real r
  integer (wp) :: i, ij, j, k, kk, l, m, nn, t, tt, tty, ty
! .. Local Arrays ..
  integer (wp) :: il(21), iu(21)
! .. Intrinsic Functions ..
  intrinsic abs, int
! ***FIRST EXECUTABLE STATEMENT  ISORT
  nn = n
!
  kk = abs(kflag)
!
! Alter array IX to get decreasing order if
! needed
!
  if (kflag<=-1) then
    do i = 1, nn
      ix(i) = -ix(i)
    end do
  end if
!
  if (kk==2) go to 180
!
! Sort IX only
!
  m = 1
  i = 1
  j = nn
  r = 0.375e0
!
100 if (i==j) go to 140
  if (r<=0.5898437e0) then
    r = r + 3.90625e-2
  else
    r = r - 0.21875e0
  end if
!
110 k = i
!
! Select a central element of the array and save
! it in location T
!
  ij = i + int(((j-i)*r), wp)
  t = ix(ij)
!
! If first element of array is greater than T,
! interchange with T
!
  if (ix(i)>t) then
    ix(ij) = ix(i)
    ix(i) = t
    t = ix(ij)
  end if
  l = j
!
! If last element of array is less than than T,
! interchange with T
!
  if (ix(j)<t) then
    ix(ij) = ix(j)
    ix(j) = t
    t = ix(ij)
!
!   If first element of array is greater than T,
!   interchange with T
!
    if (ix(i)>t) then
      ix(ij) = ix(i)
      ix(i) = t
      t = ix(ij)
    end if
  end if
!
! Find an element in the second half of the
! array which is smaller
! than T
!
120 l = l - 1
  if (ix(l)>t) go to 120
!
! Find an element in the first half of the array
! which is greater
! than T
!
130 k = k + 1
  if (ix(k)<t) go to 130
!
! Interchange these elements
!
  if (k<=l) then
    tt = ix(l)
    ix(l) = ix(k)
    ix(k) = tt
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
  t = ix(i+1)
  if (ix(i)<=t) go to 160
  k = i
!
170 ix(k+1) = ix(k)
  k = k - 1
  if (t<ix(k)) go to 170
  ix(k+1) = t
  go to 160
!
! Sort IX and carry IY along
!
180 m = 1
  i = 1
  j = nn
  r = 0.375e0
!
190 if (i==j) go to 230
  if (r<=0.5898437e0) then
    r = r + 3.90625e-2
  else
    r = r - 0.21875e0
  end if
!
200 k = i
!
! Select a central element of the array and save
! it in location T
!
  ij = i + int(((j-i)*r), wp)
  t = ix(ij)
  ty = iy(ij)
!
! If first element of array is greater than T,
! interchange with T
!
  if (ix(i)>t) then
    ix(ij) = ix(i)
    ix(i) = t
    t = ix(ij)
    iy(ij) = iy(i)
    iy(i) = ty
    ty = iy(ij)
  end if
  l = j
!
! If last element of array is less than T,
! interchange with T
!
  if (ix(j)<t) then
    ix(ij) = ix(j)
    ix(j) = t
    t = ix(ij)
    iy(ij) = iy(j)
    iy(j) = ty
    ty = iy(ij)
!
!   If first element of array is greater than T,
!   interchange with T
!
    if (ix(i)>t) then
      ix(ij) = ix(i)
      ix(i) = t
      t = ix(ij)
      iy(ij) = iy(i)
      iy(i) = ty
      ty = iy(ij)
    end if
  end if
!
! Find an element in the second half of the
! array which is smaller
! than T
!
210 l = l - 1
  if (ix(l)>t) go to 210
!
! Find an element in the first half of the array
! which is greater
! than T
!
220 k = k + 1
  if (ix(k)<t) go to 220
!
! Interchange these elements
!
  if (k<=l) then
    tt = ix(l)
    ix(l) = ix(k)
    ix(k) = tt
    tty = iy(l)
    iy(l) = iy(k)
    iy(k) = tty
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
  t = ix(i+1)
  ty = iy(i+1)
  if (ix(i)<=t) go to 250
  k = i
!
260 ix(k+1) = ix(k)
  iy(k+1) = iy(k)
  k = k - 1
  if (t<ix(k)) go to 260
  ix(k+1) = t
  iy(k+1) = ty
  go to 250
!
! Clean up
!
270 if (kflag<=-1) then
    do i = 1, nn
      ix(i) = -ix(i)
    end do
  end if
  return
end subroutine
