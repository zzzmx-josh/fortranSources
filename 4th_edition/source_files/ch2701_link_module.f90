module link_module
  use precision_module
  type link(real_kind)
    integer, kind :: real_kind
    real (kind=real_kind) :: n
    type (link(real_kind)), pointer :: next
  end type
end module
