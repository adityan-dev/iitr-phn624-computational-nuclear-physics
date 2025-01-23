program main

  use m_kinds, only : wp, dint
  use m_harmonic_oscillator, only : wave_function

  implicit none


  integer(dint), parameter :: resolution = 1000
  integer(dint), parameter :: to_order = 10
  real(wp), parameter :: limit_l = -8.0_wp
  real(wp), parameter :: limit_r = 8.0_wp

  integer(dint) :: io, i
  real(wp), dimension(resolution) :: x
  real(wp), dimension(resolution, 0:to_order) :: psi

  x = [(limit_l + (limit_r - limit_l) * ((i - 1) / (real(resolution, wp) - 1.0_wp)), i = 1, resolution)]
  do i = 0, to_order
    psi(:, i) = wave_function(i, x)
  end do
  open(newunit=io, file = "howf.dat")
  do i = 1, size(x)
     write(io, *) x(i), psi(i, :)
  enddo
  close(io)

end program main
