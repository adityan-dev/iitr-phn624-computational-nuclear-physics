module m_polynomials

  use m_kinds, only : wp, dint
  use m_assert, only : assert

  implicit none
  private

  public :: hermite, legendre

contains

  impure recursive real(wp) elemental function hermite (n, x) result (H)
    integer(dint), intent(in) :: n
    real(wp), intent(in) :: x
    call assert(n >= 0_dint, "Invalid order")
    if (n .eq. 0_dint) then
       H = 1.0_wp
    else if (n .eq. 1_dint) then
       H = 2.0_wp * x
    else
       H = 2.0_wp * x * hermite(n - 1_dint, x) - 2.0_wp * real(n - 1_dint, wp) * hermite(n - 2_dint, x)
    endif
  end function hermite

  impure recursive real(wp) elemental function legendre (n, x) result (P)
    integer(dint), intent(in) :: n
    real(wp), intent(in) :: x
    call assert(n >= 0_dint, "Invalid order")
    if (n .eq. 0_dint) then
       P = 1.0_wp
    else if (n .eq. 1_dint) then
       P = x
    else
       P = real(2 * n + 1_dint, wp) * x * legendre(n - 1_dint, x) - real(n, wp) * legendre(n - 2_dint, x)
       P = P / real(n, wp)
    endif
  end function legendre

end module m_polynomials
