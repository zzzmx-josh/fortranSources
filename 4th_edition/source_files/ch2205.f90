module tree_node_module
  implicit none

  type tree_node
    integer :: number
    type (tree_node), pointer :: left => null(), right => null()
  end type

end module

module tree_module
  implicit none

contains

  recursive function tree(n) result (answer)
    use tree_node_module
    implicit none
    integer, intent (in) :: n
    type (tree_node), pointer :: answer
    type (tree_node), pointer :: new_node
    integer :: l, r, x

    if (n==0) then
      print *, ' terminate tree'
      nullify (answer)
    else
      l = n/2
      r = n - l - 1
      print *, l, r, n
      print *, ' next item'
      read *, x
      allocate (new_node)
      new_node%number = x
      print *, ' left branch'
      new_node%left => tree(l)
      print *, ' right branch'
      new_node%right => tree(r)
      answer => new_node
    end if
    print *, ' function tree ends'
  end function

end module

module print_tree_module
  implicit none

contains

  recursive subroutine print_tree(t, h)
    use tree_node_module
    implicit none
    type (tree_node), pointer :: t
    integer :: i
    integer :: h

    if (associated(t)) then
      call print_tree(t%left, h+1)
      do i = 1, h
        write (unit=*, fmt=100, advance='no')
      end do
      print *, t%number
      call print_tree(t%right, h+1)
    end if
100 format (' ')

  end subroutine

end module

program ch2205
! construction of a perfectly balanced tree
  use tree_node_module
  use tree_module
  use print_tree_module
  implicit none
  type (tree_node), pointer :: root
  integer :: n_of_items

  print *, 'enter number of items'
  read *, n_of_items
  root => tree(n_of_items)
  call print_tree(root, 0)
end program
