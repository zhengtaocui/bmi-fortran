program test_get_grid_spacing

  use bmiheatf
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 2
  real, dimension(rank), parameter :: expected_spacing = [1.0, 1.0]

  type (bmi_heat) :: m
  real, dimension(2) :: grid_spacing
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_spacing(grid_id, grid_spacing)
  status = m%finalize()

  do i = 1, rank
     if (grid_spacing(i).ne.expected_spacing(i)) then
        stop 1
     end if
  end do
end program test_get_grid_spacing
