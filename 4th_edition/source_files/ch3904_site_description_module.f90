module site_description_module

  type site_description
    character *15 :: site_name = ' '
    character *7 :: easting_1 = ' '
    character *7 :: northing_1 = ' '
    real :: lat_1 = 0.0
    real :: long_1 = 0.0
    integer :: height_1 = 0
    character *7 :: easting_2 = ' '
    character *7 :: northing_2 = ' '
    real :: lat_2 = 0.0
    real :: long_2 = 0.0
    integer :: height_2 = 0
    integer :: start_date_month_1 = 0
    integer :: start_date_year_1 = 0
    integer :: end_date_month_1 = 0
    integer :: end_date_year_1 = 0
    integer :: start_date_month_2 = 0
    integer :: start_date_year_2 = 0
    integer :: end_date_month_2 = 0
    integer :: end_date_year_2 = 0
  end type

end module
