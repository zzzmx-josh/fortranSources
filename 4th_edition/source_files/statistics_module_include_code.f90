
  l = 1
  r = n
  do while (l<r)
    t1 = y(k)
    i = l
    j = r
    do
      do while (y(i)<t1)
        i = i + 1
      end do
      do while (t1<y(j))
        j = j - 1
      end do
      if (i<=j) then
        t2 = y(i)
        y(i) = y(j)
        y(j) = t2
        i = i + 1
        j = j - 1
      end if
      if (i>j) exit
    end do
    if (j<k) then
      l = i
    end if
    if (k<i) then
      r = j
    end if
  end do
  find = y(k)


