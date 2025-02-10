module m_polynomials

  use m_kinds, only : wp, dint
  use m_assert, only : assert
  use m_factorial, only : factorial
  use m_double_factorial, only : double_factorial

  implicit none
  private

  public :: hermite, legendre, laguerre
  public :: associated_legendre, associated_laguerre

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
       P = real(2 * n - 1_dint, wp) * x * legendre(n - 1_dint, x) - real(n - 1_dint, wp) * legendre(n - 2_dint, x)
       P = P / real(n, wp)
    endif
  end function legendre

  impure recursive real(wp) elemental function associated_legendre (l, m, x) result (P)
    integer(dint), intent(in) :: l, m
    real(wp), intent(in) :: x
    call assert(l >= 0_dint, "Invalid order")
    call assert(abs(m) <= l, "Invalid degree")
    call assert((-1.0_wp <= x) .and. (x <= 1.0_wp), "Invalid x")
    if (l .eq. 0) then
       P = 1.0_wp
    elseif (m < 0) then
       P = (-1.0_wp ** abs(real(m, wp))) * (real(factorial(l - abs(m)), wp) / real(factorial(l + abs(m)), wp)) * associated_legendre(l, -m, x)
    elseif (m .eq. l) then
       P = (-1.0_wp ** real(l, wp)) * real(double_factorial(2 * l - 1), wp) * ((1.0_wp - x ** 2.0_wp) ** (real(l, wp) / 2.0_wp))
    elseif (m .eq. l - 1) then
       P = x * real(2 * l - 1, wp) * associated_legendre(l - 1 , l - 1, x)
    else
       P = x * real(2 * l - 1, wp) * associated_legendre(l - 1, m, x) - real(l + m - 1, wp) * associated_legendre(l - 2, m, x)
       P = P / real(l - m, wp)
    endif
  end function associated_legendre


  impure recursive real(wp) elemental function laguerre (n, x) result (L)
    integer(dint), intent(in) :: n
    real(wp), intent(in) :: x
    call assert(n >= 0_dint, "Invalid order")
    if (n .eq. 0_dint) then
       L = 1.0_wp
    else if (n .eq. 1_dint) then
       L = 1.0_wp - x
    else
       L = (2.0_wp * real(n, wp) - x - 1.0_wp) * laguerre(n - 1_dint, x) - (real(n, wp) - 1.0_wp) * laguerre(n - 2_dint, x)
       L = L / real(n, wp)
    endif
  end function laguerre

  impure recursive real(wp) elemental function associated_laguerre (n, l, x) result (La)
    integer(dint), intent(in) :: n, l
    real(wp), intent(in) :: x
    call assert(n >= 0_dint, "Invalid order")
    call assert(l >= 0_dint .and. l <= n, "Invalid degree")
    if (n .eq. 0_dint) then
       La = 1.0_wp
    else if (n .eq. 1_dint) then
       La = 1.0_wp - x + l
    else
       La = (real(2 * n - 1_dint, wp) - x + l) * laguerre(n - 1_dint, x) - real(n + l - 1_dint, wp) * laguerre(n - 2_dint, x)
       La = La / real(n, wp)
    endif
  end function associated_laguerre

end module m_polynomials
