program main

  use m_kinds, only : wp
  use m_assert, only : assert

  implicit none

  real(wp), parameter :: A = 36

  real(wp), parameter :: A3 = A ** (-1.0_wp / 3.0_wp)
  real(wp), parameter :: A23 = A3 * A3
  real(wp) :: b1, b2, delta

  b1 = 41.0_wp * A3 ! MeV
  b2 = 45.0_wp * A3 - 25.0_wp * A23
  delta = b1 - b2

  print *, "B1 = b[41 A^(-1/3)] = ", b1, "MeV"
  print *, "B2 = b[41 A^(-1/3) - 25 A^(-2/3)] = ", b2, "MeV"
  print *, "DELTA = B1 - B2 = ", delta, "MeV"

contains

  real(wp) function occilator_length_parameter (E)
    real(wp), intent(in) :: E
    call assert(E > 0.0_wp, "Invalid Energy Value")
    occilator_length_parameter = 197.33_wp / sqrt(940.0_wp * E)
  end function occilator_length_parameter

end program main
