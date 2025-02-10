program main

  use m_kinds, only : wp, dint
  use m_polynomials, only : associated_legendre

  implicit none

  integer(dint), parameter :: resolution = 1000
  integer(dint), parameter :: to_order = 5
  real(wp), parameter :: limit_l = -1.0_wp
  real(wp), parameter :: limit_r = 1.0_wp

  integer(dint) :: io, l, m, j, i
  character(len=100) :: filename, file_id
  real(wp), dimension(resolution) :: x
  real(wp), dimension(resolution, -to_order:to_order) :: P

  x = [(limit_l + (limit_r - limit_l) * ((i - 1) / (real(resolution, wp) - 1.0_wp)), i = 1, resolution)]

  do l = 0, to_order
     P = 0.0_wp
     do m = -l, l
        P(:, m) = associated_legendre(l, m, x)
     end do
     write(file_id, "(I10)") l
     write(filename, "(A,A,A)") "./data/legendre_l_", trim(adjustl(file_id)), ".dat"
     open(newunit=io, file = filename)
     do j = 1, size(x)
        write(io, *) x(j), P(j, -l:l)
     enddo
     close(io)
  end do
contains


end program main
