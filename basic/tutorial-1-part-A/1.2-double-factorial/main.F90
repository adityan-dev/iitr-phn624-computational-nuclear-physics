program main

  use m_kinds, only : dint
  use m_double_factorial, only : double_factorial

  implicit none

  print *, double_factorial(3_dint)
  print *, double_factorial(4_dint)
  print *, double_factorial(22_dint)
  ! print *, double_factorial(50_dint)
  ! print *, double_factorial(-3_dint)
  ! print *, double_factorial(0_dint)
  ! print *, double_factorial(3.001_dint)


end program main
