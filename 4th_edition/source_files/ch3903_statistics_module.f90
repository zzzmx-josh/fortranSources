module statistics_module

  implicit none

contains

  subroutine calculate_month_averages(x, n, n_months, sum_x, average_x, index_by_month, month_names)

    implicit none

    real, dimension (:), intent (in) :: x
    integer, intent (in) :: n
    integer, intent (in) :: n_months

    real, dimension (1:n_months), intent (inout) :: sum_x
    real, dimension (1:n_months), intent (inout) :: average_x

    integer, dimension (1:n), intent (in) :: index_by_month
    character *9, dimension (1:n_months), intent (in) :: month_names

    integer, dimension (1:n_months) :: n_missing
    integer, dimension (1:n_months) :: n_actual

    integer :: m

    sum_x = 0.0
    average_x = 0.0
    n_missing = 0
    n_actual = 0

    do m = 1, n
      if (x(m)>-98.9) then
        sum_x(index_by_month(m)) = sum_x(index_by_month(m)) + x(m)
        n_actual(index_by_month(m)) = n_actual(index_by_month(m)) + 1
      else
        n_missing(index_by_month(m)) = n_missing(index_by_month(m)) + 1
      end if
    end do

    do m = 1, n_months
      average_x(m) = sum_x(m)/(n_actual(m))
    end do

    print *, ' Summary of actual    missing'
    print *, '            values    values'
    do m = 1, n_months
      print 100, month_names(m), n_actual(m), n_missing(m)
100   format (2x, a9, 2x, i6, 2x, i6)
    end do

  end subroutine

end module
