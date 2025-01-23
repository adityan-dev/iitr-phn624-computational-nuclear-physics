module m_factorial

  use m_kinds, only : dint
  use m_assert, only : assert

  implicit none
  private

  public :: factorial

contains

  impure elemental integer(dint) function factorial(n) result (out)
    integer(dint), intent(in) :: n
    integer(dint) :: i
    call assert(n >= 0_dint, "Input must be an whole number.")
    out = 1_dint; i = n
    do while (i > 1_dint)
       out = out * i
       i = i - 1_dint
    end do
  end function factorial

end module m_factorial
