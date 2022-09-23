module statistics_module

  use precision_module

  interface calculate_statistics
    module procedure calculate_sp
    module procedure calculate_dp
    module procedure calculate_qp
  end interface

contains


  subroutine calculate_sp(x, n, mean, std_dev, median)
    implicit none
    integer, intent (in) :: n
    real (sp), intent (in), dimension (:) :: x
    real (sp), intent (out) :: mean
    real (sp), intent (out) :: std_dev
    real (sp), intent (out) :: median
    real (sp), dimension (1:n) :: y
    real (sp) :: variance
    real (sp) :: sumxi, sumxi2

    sumxi = 0.0
    sumxi2 = 0.0
    variance = 0.0
    sumxi = sum(x)
    sumxi2 = sum(x*x)
    mean = sumxi/n
    variance = (sumxi2-sumxi*sumxi/n)/(n-1)
    std_dev = sqrt(variance)
    y = x
    if (mod(n,2)==0) then
      median = (find(n/2)+find((n/2)+1))/2
    else
      median = find((n/2)+1)
    end if

  contains

    function find(k)
      implicit none
      real (sp) :: find
      integer, intent (in) :: k
      integer :: l, r, i, j
      real (sp) :: t1, t2
      include 'statistics_module_include_code.f90'
    end function
  end subroutine

  subroutine calculate_dp(x, n, mean, std_dev, median)
    implicit none
    integer, intent (in) :: n
    real (dp), intent (in), dimension (:) :: x
    real (dp), intent (out) :: mean
    real (dp), intent (out) :: std_dev
    real (dp), intent (out) :: median
    real (dp), dimension (1:n) :: y
    real (dp) :: variance
    real (dp) :: sumxi, sumxi2

    sumxi = 0.0
    sumxi2 = 0.0
    variance = 0.0
    sumxi = sum(x)
    sumxi2 = sum(x*x)
    mean = sumxi/n
    variance = (sumxi2-sumxi*sumxi/n)/(n-1)
    std_dev = sqrt(variance)
    y = x
    if (mod(n,2)==0) then
      median = (find(n/2)+find((n/2)+1))/2
    else
      median = find((n/2)+1)
    end if
  contains
    function find(k)
      implicit none
      real (dp) :: find
      integer, intent (in) :: k
      integer :: l, r, i, j
      real (dp) :: t1, t2
      include 'statistics_module_include_code.f90'
    end function
  end subroutine


  subroutine calculate_qp(x, n, mean, std_dev, median)
    implicit none
    integer, intent (in) :: n
    real (qp), intent (in), dimension (:) :: x
    real (qp), intent (out) :: mean
    real (qp), intent (out) :: std_dev
    real (qp), intent (out) :: median
    real (qp), dimension (1:n) :: y
    real (qp) :: variance
    real (qp) :: sumxi, sumxi2

    sumxi = 0.0
    sumxi2 = 0.0
    variance = 0.0
    sumxi = sum(x)
    sumxi2 = sum(x*x)
    mean = sumxi/n
    variance = (sumxi2-sumxi*sumxi/n)/(n-1)
    std_dev = sqrt(variance)
    y = x
    if (mod(n,2)==0) then
      median = (find(n/2)+find((n/2)+1))/2
    else
      median = find((n/2)+1)
    end if
  contains
    function find(k)
      implicit none
      real (qp) :: find
      integer, intent (in) :: k
      integer :: l, r, i, j
      real (qp) :: t1, t2
      include 'statistics_module_include_code.f90'
    end function
  end subroutine

end module
