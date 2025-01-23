program main

  use m_kinds, only : dint
  use m_assert, only : assert

  implicit none

  print *, sum_of_squares(10_dint)
  ! print *, sum_of_squares(-3_dint)

contains

  integer(dint) function sum_of_squares (n) result (out)
    integer(dint), intent(in) :: n
    integer(dint) :: i
    call assert(n > 0_dint, "Input must be a natural number")
    out = 0_dint
    do i = 1_dint, n
       out = out + i ** 2_dint
    end do
  end function sum_of_squares

end program main
