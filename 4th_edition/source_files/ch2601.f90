module sparse_vector_module
  implicit none
  type sparse_vector
    integer :: index
    real :: value
    type (sparse_vector), pointer :: next => null()
  end type
end module


module read_data_module

  implicit none

contains

  subroutine read_data(filename, root_z, ifail)

    use sparse_vector_module

    implicit none
    type (sparse_vector), pointer, intent (inout) :: root_z
    character (len=*), intent (inout) :: filename
    integer, intent (inout) :: ifail
    integer :: io_status
    type (sparse_vector), pointer :: current_z

    ifail = 0

!   open file for reading data and read 1st
!   entry

    open (unit=1, file=filename, status='old', iostat=io_status)
    if (io_status/=0) then
      ifail = 1
      return
    end if
    allocate (root_z)
    read (unit=1, fmt=*, iostat=io_status) root_z%value, root_z%index
    if (io_status/=0) then
      ifail = 2
      return
    end if

!   read data from file until eof

    current_z => root_z
    allocate (current_z%next)
    do while (associated(current_z%next))
      current_z => current_z%next
      read (unit=1, fmt=*, iostat=io_status) current_z%value, current_z%index
      if (io_status==0) then
        allocate (current_z%next)
        cycle
      else if (io_status>0) then
        ifail = 3
      end if
    end do
    close (unit=1)

    return
  end subroutine

end module


program ch2601

! this program reads the non-zero elements of
! two sparse vectors x and y together with
! their indices, and stores them in two
! linked lists. using these linked lists it
! then calculates and prints out the inner
! product. it also prints the values.

  use sparse_vector_module
  use read_data_module

  implicit none
  character (len=30) :: filename

  type (sparse_vector), pointer :: root_x, current_x, root_y, current_y
  real :: inner_prod = 0.0
  integer :: ifail = 0

! ask for name of file containing vector x
! non-zero values and indices

  print *, 'input file name for vector x'
  read '(a)', filename

! read vector x non-zero elements and indices
! into a linked list

  call read_data(filename, root_x, ifail)

  if (ifail==1) then
    print *, 'error opening file ', filename
    stop 10
  else if (ifail==2) then
    print *, 'error reading from beginning of file ', filename
    stop 20
  else if (ifail==3) then
    print *, 'error reading from file ', filename
    stop 30
  end if

! ask for name of file containing vector y
! non-zero values and indices

  print *, 'input file name for vector y'
  read '(a)', filename

! read vector y non-zero elements and indices
! into a linked list

  call read_data(filename, root_y, ifail)
  if (ifail==1) then
    print *, 'error opening file ', filename
    stop 40
  else if (ifail==2) then
    print *, 'error reading from beginning of file ', filename
    stop 50
  else if (ifail==3) then
    print *, 'error reading from file ', filename
    stop 60
  end if

! data has now been read and stored in 2 linked
! lists. start at the beginning of x linked list
! and y linked list and compare indices
! in order to perform inner product

  current_x => root_x
  current_y => root_y
  do while (associated(current_x%next))
    do while (associated(current_y%next) .and. current_y%index<current_x%index)

!     move through y list

      current_y => current_y%next
    end do

!   at this point
!   current_y%index >= current_x%index
!   or 2nd list is exhausted

    if (current_y%index==current_x%index) then
      inner_prod = inner_prod + current_x%value*current_y%value
    end if
    current_x => current_x%next
  end do

! print non-zero values of vector x and indices

  print *, 'non-zero values of vector x and indices:'
  current_x => root_x
  do while (associated(current_x%next))
    print *, current_x%value, current_x%index
    current_x => current_x%next
  end do

! print non-zero values of vector y and indices

  print *, 'non-zero values of vector y and indices:'
  current_y => root_y
  do while (associated(current_y%next))
    print *, current_y%value, current_y%index
    current_y => current_y%next
  end do

! print out inner product

  print *, 'inner product of two sparse vectors is :', inner_prod

end program
