program ch3402

  implicit none

  integer :: i
  character (len=20) :: name[ * ] = '*****'

  print 100, name, this_image()

  if (this_image()==1) then
    print *, ' Type in your name'
    read *, name
    do i = 2, num_images()
      name[ i ] = name
    end do
  end if

  sync all

  print 100, name, this_image()
100 format (1x, ' Hello ', a20, ' from image ', i3)

end program
