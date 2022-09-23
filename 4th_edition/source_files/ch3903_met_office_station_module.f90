module met_office_station_module

  implicit none

  type station_type

    integer :: year
    integer :: month
    real :: tmax
    real :: tmin
    integer :: af_days
    real :: rainfall
    real :: sunshine

  end type

! Number of stations

  integer, parameter :: n_stations = 37

! Number of lines per station, read in later

  integer, dimension (n_stations) :: nl = 0

! Site names

  character *15, dimension (n_stations) :: site_name = (/ 'aberporth      ', 'armagh         ', 'ballypatrick   ', &
    'bradford       ', 'braemar        ', 'camborne       ', 'cambridge      ', 'cardiff        ', 'chivenor       ', &
    'cwmystwyth     ', 'dunstaffnage   ', 'durham         ', 'eastbourne     ', 'eskdalemuir    ', 'heathrow       ', &
    'hurn           ', 'lerwick        ', 'leuchars       ', 'lowestoft      ', 'manston        ', 'nairn          ', &
    'newtonrigg     ', 'oxford         ', 'paisley        ', 'ringway        ', 'rossonwye      ', 'shawbury       ', &
    'sheffield      ', 'southampton    ', 'stornoway      ', 'suttonbonington', 'tiree          ', 'valley         ', &
    'waddington     ', 'whitby         ', 'wickairport    ', 'yeovilton      ' /)


! Station data file names

  character *23, dimension (n_stations) :: station_data_file_name = (/ 'aberporthdata.txt      ', 'armaghdata.txt         ', &
    'ballypatrickdata.txt   ', 'bradforddata.txt       ', 'braemardata.txt        ', 'cambornedata.txt       ', &
    'cambridgedata.txt      ', 'cardiffdata.txt        ', 'chivenordata.txt       ', 'cwmystwythdata.txt     ', &
    'dunstaffnagedata.txt   ', 'durhamdata.txt         ', 'eastbournedata.txt     ', 'eskdalemuirdata.txt    ', &
    'heathrowdata.txt       ', 'hurndata.txt           ', 'lerwickdata.txt        ', 'leucharsdata.txt       ', &
    'lowestoftdata.txt      ', 'manstondata.txt        ', 'nairndata.txt          ', 'newtonriggdata.txt     ', &
    'oxforddata.txt         ', 'paisleydata.txt        ', 'ringwaydata.txt        ', 'rossonwyedata.txt      ', &
    'shawburydata.txt       ', 'sheffielddata.txt      ', 'southamptondata.txt    ', 'stornowaydata.txt      ', &
    'suttonboningtondata.txt', 'tireedata.txt          ', 'valleydata.txt         ', 'waddingtondata.txt     ', &
    'whitbydata.txt         ', 'wickairportdata.txt    ', 'yeoviltondata.txt      ' /)

! cwmystwyth   1959 - 2011
! ringway      1946 - 2004
! southampton  1855 - 2000

! default header line count

  integer, dimension (1:n_stations) :: hl = 7

  integer, parameter :: n_months = 12

  character *9, dimension (1:n_months) :: month_names = (/ 'January  ', 'February ', 'March    ', 'April    ', 'May      ', &
    'June     ', 'July     ', 'August   ', 'September', 'October  ', 'November ', 'December ' /)

contains

  subroutine initialise_station_data()
    implicit none

    integer :: i

!   Braemar, Lowestoft, Nairn, Southampton,
!   Whitby
!   have 8 header lines, as the position of
!   the station moved.

    hl(5) = 8
    hl(19) = 8
    hl(21) = 8
    hl(29) = 8
    hl(35) = 8

!   Next read in the current number of
!   lines per station
!   This changes as the data is collected,
!   and when you
!   run the C# program that gets the files.
!
!   I generate this information using wc on the
!   data files.

    open (unit=100, file='line_count.txt', status='old')

    do i = 1, n_stations
      read (100, 100) nl(i)
100   format (i7)
      nl(i) = nl(i) - hl(i)
      print 110, station_data_file_name(i), nl(i)
110   format (' Station ', a30, ' = ', i6, ' records')
    end do

    close (100)

  end subroutine

  subroutine skip_header_lines(j)

    implicit none
    integer, intent (in) :: j
    integer :: i

!   Skip header lines

    do i = 1, hl(j)
      read (unit=100, fmt='(a)')
    end do

  end subroutine

end module
