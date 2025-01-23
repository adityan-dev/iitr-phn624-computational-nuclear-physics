program main

  use m_kinds, only : wp, dint
  use m_polynomials, only : associated_legendre

  implicit none

  integer(dint), parameter :: resolution = 1000
  integer(dint), parameter :: to_order = 10
  real(wp), parameter :: limit_l = -3.0_wp
  real(wp), parameter :: limit_r = 3.0_wp

  integer(dint) :: io, i, m, j
  character(len=100) :: filename
  real(wp), dimension(resolution) :: x
  real(wp), dimension(resolution, -to_order:to_order) :: P

  x = [(limit_l + (limit_r - limit_l) * ((i - 1) / (real(resolution, wp) - 1.0_wp)), i = 1, resolution)]

  do i = 0, to_order
     P = 0.0_wp
     do m = -i, i
        print *, i, m
        P(:, i) = associated_legendre(i, m, x)
     end do
     write(filename, "(A,I3,A)") "./data/legendre_l_", i, ".dat"
     open(newunit=io, file = filename)
     do j = 1, size(x)
        write(io, *) x(j), P(j, :)
     enddo
     close(io)
  end do
contains


end program main
