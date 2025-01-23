module m_integration_1D

  use m_kinds, only : wp, dint

  implicit none
  private

  public :: integ_trapezoidal, integ_simpsons

  abstract interface
     real(wp) function generic_1D_function (x)
       import :: wp
       real(wp), intent(in) :: x
     end function generic_1D_function
  end interface

contains

  real(wp) function integ_trapezoidal(f, limit_l, limit_r, subdiv) result(integral)
    procedure(generic_1D_function) :: f
    real(wp), intent(in) :: limit_l, limit_r
    integer(dint), intent(in) :: subdiv
    integer(dint) :: i
    real(wp) :: dx
    dx = (limit_r - limit_l) / real(subdiv, wp)
    integral = f(limit_l) + f(limit_r)
    do i = 1, subdiv - 1
       integral = integral + 2.0_wp * f(real(i,wp) * dx)
    end do
    integral = dx * integral / 2.0_wp
  end function integ_trapezoidal

  real(wp) function integ_simpsons(f, limit_l, limit_r, subdiv) result(integral)
    procedure(generic_1D_function) :: f
    real(wp), intent(in) :: limit_l, limit_r
    integer(dint), intent(in) :: subdiv
    integer(dint) :: i
    real(wp) :: dx
    dx = (limit_r - limit_l) / real(subdiv, wp)
    integral = f(limit_l) + f(limit_r)
    do i = 1, subdiv - 1, 2
       integral = integral + 4.0_wp * f(real(i,wp) * dx)
    end do
    do i = 2, subdiv - 1, 2
       integral = integral + 2.0_wp * f(real(i,wp) * dx)
    end do
    integral = dx * integral / 3.0_wp
  end function integ_simpsons

end module m_integration_1D
