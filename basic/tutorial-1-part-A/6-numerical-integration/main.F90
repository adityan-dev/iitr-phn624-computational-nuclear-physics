program main

  use m_kinds, only : wp, dint
  use m_integration_1D, only: integ_trapezoidal, integ_simpsons

  implicit none

  real(wp), parameter :: limit_l = 0.0_wp
  real(wp), parameter :: limit_r = 2.0_wp
  integer(dint), parameter :: subdiv = 1000_dint

  real(wp) :: trapz, simpson
  trapz = integ_trapezoidal(f, limit_l, limit_r, subdiv)
  simpson = integ_simpsons(f, limit_l, limit_r, subdiv)

  print *, "Trapezoidal Method : ", trapz
  print *, "Simpsons Method    : ", simpson

contains

  pure real(wp) function f (x)
    real(wp), intent(in) :: x
    real(wp) :: x2
    x2 = (x ** 2.0_wp)
    f =  x2 * exp(-x2)
  end function f

end program main
