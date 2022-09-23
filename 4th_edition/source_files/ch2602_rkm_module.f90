module rkm_module
  use precision_module, wp => dp
  implicit none

contains
  subroutine runge_kutta_merson(y, fun, ifail, n, a, b, tol)
!
!   runge-kutta-merson method for the solution
!   of a system of n 1st order initial value
!   ordinary differential equations.
!   the routine tries to integrate from
!   t=a to t=b with initial conditions in y,
!   subject to the condition that the
!   absolute error estimate <= tol. the step
!   length is adjusted automatically to meet
!   this condition.
!   if the routine is successful it returns with
!   ifail = 0, t=b and the solution in y.
!
    implicit none

!   define arguments

    real (wp), intent (inout), dimension (:) :: y
    real (wp), intent (in) :: a, b, tol
    integer, intent (in) :: n
    integer, intent (out) :: ifail

    interface
      subroutine fun(t, y, f, n)
        use precision_module, wp => dp
        implicit none
        real (wp), intent (in), dimension (:) :: y
        real (wp), intent (out), dimension (:) :: f
        real (wp), intent (in) :: t
        integer, intent (in) :: n
      end subroutine
    end interface

!   local variables

    real (wp), dimension (1:size(y)) :: s1, s2, s3, s4, s5, new_y_1, new_y_2, error
    real (wp) :: t, h, h2, h3, h6, h8, factor = 1.e-2_wp
    real (wp) :: smallest_step = 1.e-6_wp, max_error
    integer :: no_of_steps = 0

    ifail = 0

!   check input parameters

    if (n<=0 .or. a==b .or. tol<=0.0) then
      ifail = 1
      return
    end if

!   initialize t to be start of interval and
!   h to be 1/100 of interval

    t = a
    h = (b-a)/100.0_wp
    do

!     ##### beginning of
!     ##### repeat loop

      h2 = h/2.0_wp
      h3 = h/3.0_wp
      h6 = h/6.0_wp
      h8 = h/8.0_wp

!     calculate s1,s2,s3,s4,s5
!     s1=f(t,y)

      call fun(t, y, s1, n)
      new_y_1 = y + h3*s1

!     s2 = f(t+h/3,y+h/3*s1)

      call fun(t+h3, new_y_1, s2, n)
      new_y_1 = y + h6*s1 + h6*s2

!     s3=f(t+h/3,y+h/6*s1+h/6*s2)

      call fun(t+h3, new_y_1, s3, n)
      new_y_1 = y + h8*(s2+3.0_wp*s3)

!     s4=f(t+h/2,y+h/8*(s2+3*s3))

      call fun(t+h2, new_y_1, s4, n)
      new_y_1 = y + h2*(s1-3.0_wp*s3+4.0_wp*s4)

!     s5=f(t+h,y+h/2*(s1-3*s3+4*s4))

      call fun(t+h, new_y_1, s5, n)

!     calculate values at t+h

      new_y_1 = y + h6*(s1+4.0_wp*s4+s5)
      new_y_2 = y + h2*(s1-3.0_wp*s3+4.0_wp*s4)

!     calculate error estimate

      error = abs(0.2_wp*(new_y_1-new_y_2))
      max_error = maxval(error)
      if (max_error>tol) then

!       halve step length and try again

        if (abs(h2)<smallest_step) then
          ifail = 2
          return
        end if
        h = h2
      else

!       accepted approximation so overwrite
!       y with y_new_1, and t with t+h

        y = new_y_1
        t = t + h

!       can next step be doubled?

        if (max_error*factor<tol) then
          h = h*2.0_wp
        end if

!       does next step go beyond interval end b,
!       if so set h = b-t

        if (t+h>b) then
          h = b - t
        end if
        no_of_steps = no_of_steps + 1
      end if
      if (t>=b) exit

!     ##### end of
!     ##### repeat loop

    end do
  end subroutine
end module
