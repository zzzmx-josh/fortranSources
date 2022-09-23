module personal_module
  implicit none
  type person
    real :: weight
    integer :: age
    character :: gender
  end type
end module
module subs_module
  use personal_module
  implicit none
contains
  subroutine read_data(data, no)
    implicit none
    type (person), dimension (:), allocatable, intent (out) :: data
    integer, intent (out) :: no
    integer :: i


    print *, 'input number of patients'
    read *, no
    allocate (data(1:no))

    do i = 1, no
      print *, 'for person ', i
      print *, 'weight ?'
      read *, data(i)%weight
      print *, 'age ?'
      read *, data(i)%age
      print *, 'gender ?'
      read *, data(i)%gender
    end do
  end subroutine
  subroutine stats(data, no, m_a, f_a)
    implicit none
    type (person), dimension (:), intent (in) :: data
    real, intent (out) :: m_a, f_a
    integer, intent (in) :: no
    integer :: i, no_f, no_m

    m_a = 0.0
    f_a = 0.0
    no_f = 0
    no_m = 0
    do i = 1, no
      if (data(i)%gender=='M' .or. data(i)%gender=='m') then
        m_a = m_a + data(i)%weight
        no_m = no_m + 1
      else if (data(i)%gender=='F' .or. data(i)%gender=='f') then
        f_a = f_a + data(i)%weight
        no_f = no_f + 1
      end if
    end do
    if (no_m>0) then
      m_a = m_a/no_m
    end if
    if (no_f>0) then
      f_a = f_a/no_f
    end if
  end subroutine
end module
program ch2103
  use personal_module
  use subs_module
  implicit none
  type (person), dimension (:), allocatable :: patient
  integer :: no_of_patients
  real :: male_average, female_average

  call read_data(patient, no_of_patients)
  call stats(patient, no_of_patients, male_average, female_average)
  print *, 'average male weight is ', male_average
  print *, 'average female weight is ', female_average
end program
