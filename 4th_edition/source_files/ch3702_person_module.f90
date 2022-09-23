module ch3702_person_module

  implicit none

  type :: person

    character (len=30) :: name
    integer :: age
    real :: height
    real :: weight

  contains

    procedure :: print_person
    generic :: write (formatted) => print_person
    procedure :: read_person
    generic :: read (formatted) => read_person

  end type

contains

  subroutine print_person(p, unit_number, iotype, vlist, iostat, iomsg)

    implicit none

    class (person), intent (in) :: p
    integer, intent (in) :: unit_number
    character (len=*), intent (in) :: iotype
    integer, dimension (:), intent (in) :: vlist
    integer, intent (out) :: iostat
    character (len=*), intent (inout) :: iomsg

    character (len=40) :: person_format

    write (person_format, 100) '(a', vlist(1), ',', 'i', vlist(2), ',2x,', 'f', vlist(3), '.', vlist(4), ',2x,', 'f', vlist(5), &
      '.0)'
100 format (a, i2, a, a, i1, a, a, i1, a, i1, a, a, i1, a)

    write (unit_number, fmt=person_format) p%name, p%age, p%height, p%weight

    iostat = 0

  end subroutine

  subroutine read_person(p, unit_number, iotype, vlist, iostat, iomsg)

    implicit none

    class (person), intent (inout) :: p
    integer, intent (in) :: unit_number
    character (len=*), intent (in) :: iotype
    integer, dimension (:), intent (in) :: vlist
    integer, intent (out) :: iostat
    character (len=*), intent (inout) :: iomsg

    character (len=40) :: person_format

    write (person_format, 100) '(a', vlist(1), ',2x,', 'i', vlist(2), ',2x,', 'f', vlist(3), '.', vlist(4), ',2x,', 'f', &
      vlist(5), '.0)'
100 format (a, i2, a, a, i1, a, a, i1, a, i1, a, a, i1, a)

    read (unit_number, fmt=person_format) p%name, p%age, p%height, p%weight

    iostat = 0

  end subroutine

end module
