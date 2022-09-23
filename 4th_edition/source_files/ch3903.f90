include 'ch3903_statistics_module.f90'
include 'ch3903_met_office_station_module.f90'

program ch3903

  use met_office_station_module
  use statistics_module

  implicit none

! met office data user defined type

  type (station_type), dimension (:), allocatable :: station_data

! Temporary variables used on the read

  integer :: year
  integer :: month
  real :: tmax
  real :: tmin
  integer :: af_days
  real :: rainfall
  real :: sunshine

! Currently we only calculate the
! rainfall sum and averages.

! real, dimension (1:n_months) :: sum_tmax
! real, dimension (1:n_months) :: sum_tmin
! real, dimension (1:n_months) :: sum_af_days
  real, dimension (1:n_months) :: sum_rainfall
! real, dimension (1:n_months) :: sum_sunshine

! real, dimension (1:n_months) :: average_tmax
! real, dimension (1:n_months) :: average_tmin
! real, dimension (1:n_months) ::
! average_af_days
  real, dimension (1:n_months) :: average_rainfall
! real, dimension (1:n_months) ::
! average_sunshine

! Table to hold the monthly rainfall averages
! for all stations.

  real, dimension (1:n_months, 1:n_stations) :: rainfall_table = 0

  integer :: n_years

  integer :: i, j

  call initialise_station_data()

! Process each station

  do j = 1, n_stations

    print *, ' '
    print *, ' Processing ', station_data_file_name(j)
    print *, ' '

    open (unit=100, file=station_data_file_name(j), status='old')

!   skip the header lines before starting to
!   read the data

    call skip_header_lines(j)

!   the number of observations at each station
!   is stored in the nl array.

    allocate (station_data(1:nl(j)))

!   Read in the data for each station

    do i = 1, nl(j)
      read (unit=100, fmt=100) year, month, tmax, tmin, af_days, rainfall, sunshine
100   format (3x, i4, 2x, i2, 2x, f5.1, 3x, f5.1, 3x, i5, 2x, f6.1, 1x, f6.1)
      station_data(i) = station_type(year, month, tmax, tmin, af_days, rainfall, sunshine)
    end do

    close (100)

!   Do the monthly average calculations
!   for each station

    call calculate_month_averages(station_data%rainfall, nl(j), n_months, sum_rainfall, average_rainfall, station_data%month, &
      month_names)

    n_years = station_data(nl(j))%year - station_data(1)%year + 1

    print *, ' '
    print *, ' Start date ', station_data(1)%year, ' ', station_data(1)%month
    print *, ' '
    print *, ' Rainfall monthly averages over'
    print 110, n_years
110 format ('  ~ ', i5, ' years          mm    ins')
    do i = 1, n_months
      print 120, month_names(i), average_rainfall(i), (average_rainfall(i)/25.4)
120   format (2x, a9, 8x, f7.2, 2x, f5.2)
    end do
    print 130, sum(average_rainfall), (sum(average_rainfall)/25.4)
130 format ('  Annual rainfall', /, '  average         ', f8.2, 2x, f5.2)
    print *, ' '
    print *, ' End date  ', station_data(nl(j))%year, ' ', station_data(nl(j))%month

    rainfall_table(1:n_months, j) = average_rainfall

!   Deallocate the arrays

    deallocate (station_data)

!   move on to next station

  end do

  print *, ' '
  print 140, site_name(1:n_stations)
140 format (37(2x,a7))
  print *, ' '

  do i = 1, n_months
    print 150, rainfall_table(i, 1:n_stations)/25.4
150 format (37(2x,f7.2))
  end do

end program

