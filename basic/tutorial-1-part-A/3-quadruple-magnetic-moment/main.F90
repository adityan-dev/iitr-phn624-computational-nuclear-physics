program main

  use m_kinds, only : wp, dint, i8
  use m_assert, only : assert

  implicit none

  integer(i8), parameter :: neutron = 1_i8
  integer(i8), parameter :: proton = 2_i8

  real(wp), parameter :: gl_p = 1.0_wp
  real(wp), parameter :: gl_n = 0.0_wp
  real(wp), parameter :: gs_p = 5.585_wp
  real(wp), parameter :: gs_n = 3.826_wp

  ! Li : Z = 3, A = 7, N = 4
  !        n               p          shell   capacity  cumulative
  !     ------ 0        ------- 0     1p1/2      2          8
  !     ------ 2        ------- 1     1p3/2      4          6
  !     ------ 2        ------- 2     1s1/2      2          2
  !     (paired)       (unpaired) -> L = 1, J = 3/2 = 1.5

  real(wp) :: Q, mu

  real(wp), parameter :: A = 7.0_wp
  real(wp), parameter :: L = 1.0_wp
  real(wp), parameter :: J = 1.5_wp

  Q = quadruple_moment(A, L, J)
  mu = magnetic_moment(L, J, proton)

  print *, "Qaudruple Moment : ", Q
  print *, "Magnetic Moment : ", mu

contains

  real(wp) function quadruple_moment (A, L, J)
    real(wp), intent(in) :: A
    real(wp), intent(in) :: L
    real(wp), intent(in) :: J
    call assert(A > 0.0_wp, "Invalid Atomic Number")
    if (abs(J - L) .ne. 0.5_wp) then
       call assert(.false., "Invalid Energy Level (L, J)")
    endif
    quadruple_moment = - (r_sq(A) / 2.0_wp) * (2.0_wp * J - 1.0_wp) / (J + 1.0_wp)
  end function quadruple_moment

  real(wp) function magnetic_moment (L, J, nucleon_type)
    real(wp), intent(in) :: L
    real(wp), intent(in) :: J
    integer(i8), intent(in) :: nucleon_type
    real(wp) :: gl, gs
    if (nucleon_type .eq. neutron) then
       gl = gl_n; gs = gs_n
    elseif (nucleon_type .eq. proton) then
       gl = gl_p; gs = gs_p
    else
       call assert(.false., "Invalid Nucleon Type")
    endif
    if (J .eq. L + 0.5_wp) then
       magnetic_moment = (J - 0.5_wp) * gl + 0.5_wp * gs
    elseif (J .eq. L - 0.5_wp) then
       magnetic_moment = ((J - 0.5_wp) * gl - 0.5_wp * gs) * (J / (J + 1.0_wp))
    else
       call assert(.false., "Invalid Energy Level (L, J)")
    endif
  end function magnetic_moment

  real(wp) function r_sq (A)
    real(wp), intent(in) :: A
    call assert(A > 0.0_wp, "Invalid Atomic Number")
    r_sq = (3.0_wp / 5.0_wp) * (1.2 * A ** (1.0_wp / 3.0_wp)) ** 2.0_wp
  end function r_sq

end program main
