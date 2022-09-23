include 'ch3904_site_description_module.f90'

program ch3904

  use site_description_module

  implicit none

  integer, parameter :: n_stations = 37

! site names

  character *15, dimension (n_stations) :: site_name = (/ 'aberporth      ', 'armagh         ', 'ballypatrick   ', &
    'bradford       ', 'braemar        ', 'camborne       ', 'cambridge      ', 'cardiff        ', 'chivenor       ', &
    'cwmystwyth     ', 'dunstaffnage   ', 'durham         ', 'eastbourne     ', 'eskdalemuir    ', 'heathrow       ', &
    'hurn           ', 'lerwick        ', 'leuchars       ', 'lowestoft      ', 'manston        ', 'nairn          ', &
    'newtonrigg     ', 'oxford         ', 'paisley        ', 'ringway        ', 'rossonwye      ', 'shawbury       ', &
    'sheffield      ', 'southampton    ', 'stornoway      ', 'suttonbonington', 'tiree          ', 'valley         ', &
    'waddington     ', 'whitby         ', 'wickairport    ', 'yeovilton      ' /)

! Braemar, Lowestoft, Nairn,
! Southampton, Whitby
! have 8 header lines, as the position
! of the station moved.

  type (site_description), dimension (1:n_stations) :: site_details

  integer :: i

  open (unit=10, file='location_line.txt', status='old')

  do i = 1, n_stations

    site_details(i)%site_name = site_name(i)
    read (unit=10, fmt=100) site_details(i)%easting_1, site_details(i)%northing_1, site_details(i)%lat_1, &
      site_details(i)%long_1, site_details(i)%height_1
100 format (10x, a6, 2x, a7, 7x, f6.3, 5x, f6.3, 2x, i3)
  end do

  close (10)

  open (unit=20, file='third_line.txt', status='old')

! Update Braemar

! print *,' Braemar'

  read (unit=20, fmt=110) site_details(5)%easting_2, site_details(5)%northing_2, site_details(5)%lat_2, site_details(5)%long_2, &
    site_details(5)%height_2

110 format (2x, a6, 2x, a6, 7x, f6.3, 5x, f6.3, 2x, i3)

  site_details(5)%end_date_month_1 = 4
  site_details(5)%end_date_year_1 = 2005
  site_details(5)%start_date_month_2 = 8
  site_details(5)%start_date_year_2 = 2005

! Update Lowestoft

! print *,' Lowestoft'

  read (unit=20, fmt=110) site_details(19)%easting_2, site_details(19)%northing_2, site_details(19)%lat_2, &
    site_details(19)%long_2, site_details(19)%height_2

  site_details(19)%end_date_month_1 = 8
  site_details(19)%end_date_year_1 = 2007
  site_details(19)%start_date_month_2 = 9
  site_details(19)%start_date_year_2 = 2007

! Update Nairn

! print *,' Nairn'

  read (unit=20, fmt=110) site_details(21)%easting_2, site_details(21)%northing_2, site_details(21)%lat_2, &
    site_details(21)%long_2, site_details(21)%height_2

  site_details(21)%end_date_month_1 = 12
  site_details(21)%end_date_year_1 = 1997
  site_details(21)%start_date_month_2 = 1
  site_details(21)%start_date_year_2 = 1998

! Update Southampton

! print *,' Southampton'

  read (unit=20, fmt=110) site_details(29)%easting_2, site_details(29)%northing_2, site_details(29)%lat_2, &
    site_details(29)%long_2, site_details(29)%height_2

  site_details(29)%end_date_month_1 = 12
  site_details(29)%end_date_year_1 = 1969
  site_details(29)%start_date_month_2 = 1
  site_details(29)%start_date_year_2 = 1970

! Update Whitby

! print *,' Whitby'

  read (unit=20, fmt=110) site_details(35)%easting_2, site_details(35)%northing_2, site_details(35)%lat_2, &
    site_details(35)%long_2, site_details(35)%height_2

  site_details(35)%end_date_month_1 = 12
  site_details(35)%end_date_year_1 = 1999
  site_details(35)%start_date_month_2 = 1
  site_details(35)%start_date_year_2 = 2000

  close (20)

! Start dates

  open (unit=30, file='first_data_line.txt', status='old')

  do i = 1, n_stations
    read (30, fmt=120) site_details(i)%start_date_year_1, site_details(i)%start_date_month_1
120 format (3x, i4, 2x, i2)
  end do

  close (30)

! End dates

  open (unit=40, file='end_data_line.txt', status='old')

  do i = 1, n_stations
    select case (i)
    case (5, 19, 21, 29, 35)
      read (40, fmt=130) site_details(i)%end_date_year_2, site_details(i)%end_date_month_2
    case default
      read (40, fmt=130) site_details(i)%end_date_year_1, site_details(i)%end_date_month_1
130   format (3x, i4, 2x, i2)
    end select
  end do

  close (40)

  do i = 1, n_stations
    print 140, site_details(i)
140 format (a15, 2x, a7, 2x, a7, 2x, f6.3, 2x, f6.3, 2x, i3, 2x, a7, 2x, a7, f6.3, 2x, f6.3, 2x, i3, 4(2x,i2,2x,i4))
  end do

end program

