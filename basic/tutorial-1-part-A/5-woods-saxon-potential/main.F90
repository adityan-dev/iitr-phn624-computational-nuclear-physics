program main

  use m_kinds, only : i32, wp
  use m_assert, only : assert

  implicit none


  real(wp), parameter :: R = 5.0_wp ! fm
  real(wp), parameter :: a = 0.5_wp
  real(wp), parameter :: V0 = -25.0_wp ! MeV
  integer(i32), parameter :: resolution = 1000
  real(wp), parameter :: limit_l = -10.0_wp
  real(wp), parameter :: limit_r = 10.0_wp

  integer(i32) :: io, i
  real(wp), dimension(resolution) :: x, V

  x = [(limit_l + (limit_r - limit_l) * ((i - 1) / (real(resolution, wp) - 1.0_wp)), i = 1, resolution)]
  V = woods_saxon_potential(x)
  open(newunit=io, file = "wood_saxon.dat")
  do i = 1, size(x)
     write(io, *) x(i), V(i)
  enddo
  close(io)

contains

  real(wp) elemental function woods_saxon_potential (input_r)
    real(wp), intent(in) :: input_r
    woods_saxon_potential = V0 / (1.0_wp + exp((abs(input_r) - R) / a))
  end function woods_saxon_potential

end program main
