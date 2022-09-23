module generic_sort_module

! use user_module , internal_type => user_type
! less_than is a logical function in the module

  use date_module, internal_type => date

  implicit none

contains

  subroutine sort(x, n)
    integer, intent (in) :: n
    type (internal_type), intent (inout), dimension (n) :: x

    call quicksort(1, n)

  contains

    recursive subroutine quicksort(l, r)
      implicit none
      integer, intent (in) :: l, r
!     local variables
      integer :: i, j
      type (internal_type) :: v, t

      i = l
      j = r
      v = x(int((l+r)/2))
      do
        do while (less_than(x(i),v))
          i = i + 1
        end do
        do while (less_than(v,x(j)))
          j = j - 1
        end do
        if (i<=j) then
          t = x(i)
          x(i) = x(j)
          x(j) = t
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
    end subroutine

  end subroutine

end module
