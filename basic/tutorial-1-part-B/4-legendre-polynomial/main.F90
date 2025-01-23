program main

  use m_kinds, only : wp, dint
  use m_polynomials, only : legendre

  implicit none


  integer(dint), parameter :: resolution = 1000
  integer(dint), parameter :: to_order = 10
  integer(dint), parameter :: to_degree = 10
  real(wp), parameter :: limit_l = -3.0_wp
  real(wp), parameter :: limit_r = 3.0_wp

  integer(dint) :: io, i
  real(wp), dimension(resolution) :: x
  real(wp), dimension(resolution, 0:to_order) :: P

  x = [(limit_l + (limit_r - limit_l) * ((i - 1) / (real(resolution, wp) - 1.0_wp)), i = 1, resolution)]
  do i = 0, to_order
    P(:, i) = hermite(i, x)
  end do
  open(newunit=io, file = "legendre.dat")
  do i = 1, size(x)
     write(io, *) x(i), P(i, :)
  enddo
  close(io)

contains


end program main
