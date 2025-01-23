module m_polynomials

  use m_kinds, only : wp, dint
  use m_assert, only : assert
  use m_factorial, only : factorial

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
    real(wp) :: sum
    integer(dint) :: k
    call assert(l >= 0_dint, "Invalid order")
    call assert(abs(m) <= l, "Invalid degree")
    if (m < 0) then
       P = (-1.0_wp ** real(m, wp)) * (real(factorial(l - m), wp) / real(factorial(l + m), wp)) * associated_legendre(l, -m, x)
    else
       sum = 0.0_wp
       do k = m, l
          sum = sum + (real(factorial((k + l - 1_dint) / 2_dint), wp) / real(factorial((k - l - 1_dint) / 2_dint), wp)) &
               * ((x ** real(k - m, wp)) / real(factorial(k - m), wp))
       end do
       P = (-1.0_wp ** real(m, wp)) * (2.0_wp ** real(l, wp)) * ((1.0_wp - x ** 2.0_wp) ** (real(m, wp) / 2.0_wp)) * sum
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
       L = (real(2 * n - 1_dint, wp) - x) * legendre(n - 1_dint, x) - real(n - 1_dint, wp) * legendre(n - 2_dint, x)
       L = L / real(n, wp)
    endif
  end function laguerre

  impure recursive real(wp) elemental function associated_laguerre (n, l, x) result (La)
    integer(dint), intent(in) :: n, l
    real(wp), intent(in) :: x
    integer(dint) :: i
    call assert(n >= 0_dint, "Invalid order")
    call assert(l >= 0_dint .and. l <= n, "Invalid degree")
    La = 0.0_wp
    do i = 0, n
       La = La + real(factorial(l + n), wp) * (-x ** real(i, wp)) / real(factorial(i) * factorial(n - i) * factorial(l + i), wp)
    enddo
  end function associated_laguerre

end module m_polynomials
