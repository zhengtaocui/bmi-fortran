program test_get_time_units

  use bmiheatf
  use fixtures, only: config_file, status

  implicit none

  character (len=*), parameter :: expected_units = "-"

  type (bmi_heat) :: m
  character (len=BMI_MAX_UNITS_NAME) :: units

  status = m%initialize(config_file)
  status = m%get_time_units(units)
  status = m%finalize()

  if (units.ne.expected_units) then
     stop 1
  end if
end program test_get_time_units
