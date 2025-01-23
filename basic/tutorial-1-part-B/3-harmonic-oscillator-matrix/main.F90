program main

  use m_kinds, only : wp, dint
  use m_harmonic_oscillator, only : wave_function, d2_dx2_wave_function
  use m_integration_1D, only : integ_simpsons

  implicit none

  ! <m|U|n> -> 1/2 mw^2 <m|x^2|n> = hbar^2 / 2m int(-inf -> inf)[psi(m,x) x^2 psi(n, x)]
  ! <m|T|n> -> - hbar^2 / 2m <m|d2/dx2|n> = - hbar^2 / 2m int(-inf -> inf)[psi(m,x) d2/dx2 psi(n, x)]

  integer(dint), parameter :: to_order = 6_dint
  integer(dint), parameter :: subdiv = 1000_dint
  real(wp), parameter :: limit_l = -8.0_wp
  real(wp), parameter :: limit_r = 8.0_wp

  real(wp), dimension(to_order, to_order) :: T, U ! hbar^2 / 2m
  integer(dint) :: i, n, m

  do m = 1, to_order
     do n = 1, to_order
        T(m, n) = -1.0_wp * integ_simpsons(T_density, limit_l, limit_r, subdiv)
        U(m, n) = integ_simpsons(U_density, limit_l, limit_r, subdiv)
     end do
  end do

  print *, "Kinetic Energy Matrix T"
  do i = 1, to_order
     write(*, *) T(i, :)
  end do
  print *, "Potential Energy Matrix U"
  do i = 1, to_order
     print *, U(i, :)
  end do

  contains

  real(wp) function T_density (x)
    real(wp), intent(in) :: x
    T_density = wave_function(m, x) * wave_function(n, x)  * x ** 2.0_wp
  end function T_density

  real(wp) function U_density (x)
    real(wp), intent(in) :: x
    U_density = wave_function(m, x) * d2_dx2_wave_function(n, x)
  end function U_density

end program main
