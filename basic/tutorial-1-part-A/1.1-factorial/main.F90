program main

  use m_kinds, only : dint
  use m_factorial, only : factorial

  implicit none

  print *, factorial(3_dint)
  ! print *, factorial(22_dint)
  ! print *, factorial(-3_dint)
  ! print *, factorial(0_dint)
  ! print *, factorial(3.001_dint)


end program main
