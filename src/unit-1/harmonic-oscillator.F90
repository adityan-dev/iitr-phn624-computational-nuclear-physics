module m_harmonic_oscillator

  use m_kinds, only : wp, dint
  use m_constants, only : pi
  use m_polynomials, only : hermite
  use m_factorial, only : factorial

  implicit none
  private

  public :: wave_function, d2_dx2_wave_function

contains

  impure real(wp) elemental function wave_function(n, x)
    real(wp), intent(in) :: x
    integer(dint), intent(in) :: n
    wave_function = exp(-0.5_wp * x ** 2.0_wp) * hermite(n, x) / (sqrt((2.0_wp ** real(n, wp)) * real(factorial(n))) * (pi ** (1.0_wp / 4.0_wp)))
  end function wave_function

  impure real(wp) elemental function d2_dx2_wave_function(n, x)
    real(wp), intent(in) :: x
    integer(dint), intent(in) :: n
    if (n .eq. 0) then
       d2_dx2_wave_function = (x ** 2.0_wp - 1.0_wp) * wave_function(n, x)
    elseif (n .eq. 1) then
       d2_dx2_wave_function = (x ** 2.0_wp - 1.0_wp) * wave_function(n, x) &
            - sqrt(8.0_wp * real(n, wp)) * x * wave_function(n - 1_dint, x)
    else
       d2_dx2_wave_function = 2.0_wp * sqrt(real(n * (n - 1_dint), wp)) * wave_function(n - 2_dint, x) &
            + (x ** 2.0_wp - 1.0_wp) * wave_function(n, x) &
            - sqrt(8.0_wp * real(n, wp)) * x * wave_function(n - 1_dint, x)
    endif
  end function d2_dx2_wave_function

end module m_harmonic_oscillator
