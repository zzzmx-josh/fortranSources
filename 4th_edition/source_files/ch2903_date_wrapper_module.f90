module date_wrapper_module

  use date_module

  type date_wrapper
    class (date), allocatable :: date
  end type

end module
