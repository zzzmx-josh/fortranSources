
  i = l
  j = r
  v = raw_data(int((l+r)/2))
  do
    do while (raw_data(i)<v)
      i = i + 1
    end do
    do while (v<raw_data(j))
      j = j - 1
    end do
    if (i<=j) then
      t = raw_data(i)
      raw_data(i) = raw_data(j)
      raw_data(j) = t
      i = i + 1
      j = j - 1
    end if
    if (i>j) exit
  end do
  if (l<j) then
    call quicksort(l, j)
  end if
  if (i<r) then
    call quicksort(i, r)
  end if

