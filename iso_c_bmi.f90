! The Basic Model Interface ISO_C_BINDINGING compatible free functions
!
! @author: Nels Frazier
! @email: nels.frazier@noaa.gov
! Date: August 23, 2021
!
! This module provides a set of ISO_C_BINDING compatable functions
! that allow a Fortran BMI compatible model to interoperate with a C program, given that the
! BMI module implelements a `register` function that is able to return an appropriate opaque handle
! to the C caller.

module iso_c_bmif_2_0
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_char, c_null_char, c_int, c_double, c_float
  implicit none

  type box
    class(bmi), pointer :: ptr => null()
  end type

  contains
    pure function c_to_f_string(c_string) result(f_string)
      implicit none
      character(kind=c_char, len=1), intent(in) :: c_string(:)
      character(len=:), allocatable :: f_string
      integer i,n

      !loop  through the c_string till terminator is found
      i = 1
      do
        if (c_string(i) == c_null_char) then 
            exit
        else
           i = i+1
        end if
      end do
      n = i - 1 ! trim terminator
      allocate(character(len=n) :: f_string)
      f_string = transfer( c_string(1:n), f_string )
    end function c_to_f_string

    pure function f_to_c_string(f_string) result(c_string)
      implicit none
      character(len=*), intent(in) :: f_string
      !Create a C compatable character array with room for a null terminator
      character(kind=c_char, len=1), dimension( len_trim(f_string) + 1 ) :: c_string

      !loop through the string, copy each char
      integer i,n
      n = len_trim(f_string)
      do i = 1, n
        c_string(i) = f_string(i:i)
      end do
      c_string(n+1) = c_null_char !make sure to add null terminator
    end function f_to_c_string

    ! Perform startup tasks for the model.
    function initialize(this, config_file) result(bmi_status) bind(C, name="initialize")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_FILE_NAME), intent(in) :: config_file
      integer(kind=c_int) :: bmi_status
      character(len=:), allocatable :: f_file 
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !convert c style string to fortran character array
      write(*,*) "Initialize: c file ",config_file
      f_file = c_to_f_string(config_file)
      write(*,*) "Initialize: f file ", f_file

      bmi_status = bmi_box%ptr%initialize(f_file)
      deallocate(f_file)
    end function initialize

    ! Advance the model one time step.
    function update(this) result(bmi_status) bind(C, name="update")
      type(c_ptr) :: this
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%update()
    end function update

    ! Advance the model until the given time.
    function update_until(this, time) result(bmi_status) bind(C, name="update_until")
      type(c_ptr) :: this
      real(kind=c_double), intent(in) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%update_until(time)
    end function update_until

    ! Perform teardown tasks for the model.
    function finalize(this) result(bmi_status) bind(C, name="finalize")
      type(c_ptr) :: this
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%finalize()
      !clean up the wrapper
      if( associated( bmi_box%ptr ) ) deallocate(bmi_box%ptr)
      if( associated( bmi_box ) ) deallocate(bmi_box)
    end function finalize

    ! Get the name of the model.
    function get_component_name(handle, name) result(bmi_status) bind(C, name="get_component_name")
      type(c_ptr) :: handle
      character(kind=c_char, len=1), dimension(*), intent(out) :: name
      character(len=BMI_MAX_COMPONENT_NAME), pointer :: f_name
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(handle, bmi_box)
      bmi_status = bmi_box%ptr%get_component_name(f_name)
      !Set the c_string input (name), make sure to inlcude the null_terminator
      name(:len_trim(f_name)+1) = f_to_c_string(f_name)
    end function get_component_name

    function get_current_time(this, time) result(bmi_status) bind(C, name="get_current_time")
      type(c_ptr) :: this
      real(kind=c_double), intent(out) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_current_time(time)
    end function get_current_time

    ! Start time of the model.
    function get_start_time(this, time) result(bmi_status) bind(C, name="get_start_time")
      type(c_ptr) :: this
      real(kind=c_double), intent(out) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_start_time(time)
    end function get_start_time

    ! End time of the model.
    function get_end_time(this, time) result(bmi_status) bind(C, name="get_end_time")
      type(c_ptr) :: this
      real(kind=c_double), intent(out) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_end_time(time)
    end function get_end_time

end module iso_c_bmif_2_0
