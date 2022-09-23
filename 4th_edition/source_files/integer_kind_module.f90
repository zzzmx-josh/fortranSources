module integer_kind_module
  implicit none
  integer, parameter :: i8 = selected_int_kind(2)
  integer, parameter :: i16 = selected_int_kind(4)
  integer, parameter :: i32 = selected_int_kind(9)
  integer, parameter :: i64 = selected_int_kind(15)
end module
