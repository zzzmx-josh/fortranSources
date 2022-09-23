program ch3404
  implicit none
  integer :: n, i, j
  integer :: me, nim, start, end
  integer, parameter :: factor = 5
  integer, dimension (1:factor), codimension [ * ] :: x

  nim = num_images()
  me = this_image()
  n = nim*factor
  x = 0
  start = factor*(me-1) + 1
  end = factor*me
  j = 1
  do i = start, end
    x(j) = i*factor
    print *, 'on image ', me, 'j = ', j, ' x(j) = ', x(j)
    j = j + 1
  end do
  sync all
  if (me==1) then
    print *, 'coarray x on image ', me, ' is: ', x
    do i = 2, nim
      print *, 'coarray x on image ', i, ' is: ', x(:)[ i ]
    end do
  end if
end program
