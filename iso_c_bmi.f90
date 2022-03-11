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
!
! Last modified by Zhengtao Cui (Zhengtao.Cui@noaa.gov)
! Last modification date: Feb 10, 2022
! Description: modified the original code for the dummy sine model to
!             various approaches and strategies to serialize Fortran
!             models.
!             Added function for testing get_var_ptr_* functions, i.e,
!             pass value to C by pointers.
!
! Last modified by Zhengtao Cui (Zhengtao.Cui@noaa.gov)
! Last modification date: Feb 23, 2022
!
! Description: Changed the iso c binding function to use pass-by-copy 
!              for the `this` argument in the functions. This is needed 
!              becasue we will combine the Fortran serialization  code
!              with C serialization code, which uses pass-by-copy for
!              C serialization functions.
!
! Last modified by Zhengtao Cui (Zhengtao.Cui@noaa.gov)
! Last modification date: Mar 10, 2022
!
! Description: Changed the get_var_names to return a copy of the names
!              instead of a pointer. It is consistent with the version
!              in ngen repository now.
!
module iso_c_bmif_2_0
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, &
          c_char, c_null_char, c_int, c_double, c_float, c_short, &
          c_long, c_bool
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
      ! we use pass-by-value here to be consistant with C bmi serialization
      ! code.
      type(c_ptr), value :: this
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
      type(c_ptr), value :: this
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%update()
    end function update

    ! Advance the model until the given time.
    function update_until(this, time) result(bmi_status) bind(C, name="update_until")
      type(c_ptr), value :: this
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
      type(c_ptr), value :: this
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
      type(c_ptr), value :: handle
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
      type(c_ptr), value :: this
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
      type(c_ptr), value :: this
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
      type(c_ptr), value :: this
      real(kind=c_double), intent(out) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_end_time(time)
    end function get_end_time

    function get_var_count(this, role, count) result(bmi_status) bind(C, name="get_var_count")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_ROLE_NAME), intent(in) :: role
      integer(kind=c_int) :: bmi_status
      integer(kind=c_int), intent(out) :: count
      type(box), pointer :: bmi_box
      character(len=:), allocatable :: f_role

      call c_f_pointer(this, bmi_box)
      f_role = c_to_f_string(role)
      bmi_status = bmi_box%ptr%get_var_count(f_role, count)
      deallocate(f_role)
    end function get_var_count

    ! Get the data type of the given variable as a string.
    function get_var_type(this, name, type) result(bmi_status) bind(C, name="get_var_type")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      character(kind=c_char, len=1), intent(out) :: type (*)
      character(kind=c_char, len=BMI_MAX_TYPE_NAME) :: f_type
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_type(f_str, f_type)
      type(1:len_trim(f_type)+1) = f_to_c_string(f_type)
      deallocate(f_str)
    end function get_var_type

    function get_var_names(this, role, names) result(bmi_status) bind(C, name="get_var_names")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_ROLE_NAME), intent(in) :: role
      type(c_ptr), intent(inout)  :: names (*)
!      type(c_ptr), intent(inout)  :: names
      character(kind=c_char, len=BMI_MAX_VAR_NAME), pointer :: f_names(:)
      character(kind=c_char, len=1), pointer :: c_buff_ptr(:)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(len=:), allocatable :: f_role
      integer :: i

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      f_role = c_to_f_string(role)

      bmi_status = bmi_box%ptr%get_var_names(f_role, f_names)

      !
      !This will return a pointer instead of a copy
      ! of the names.
      !
!      do i = 1, size(f_names)
!         f_names(i) = trim(f_names(i))//c_null_char
!      end do
!      names = c_loc(f_names(1))
      do i = 1, size(f_names)
        !For each pointer (one for each name), associate c_buff_ptr with the string names points to
        call c_f_pointer(names(i), c_buff_ptr, [ BMI_MAX_VAR_NAME ] )
        !assign the c_string to buffer
        c_buff_ptr = f_to_c_string(f_names(i))
      end do
    end function get_var_names

    function get_var_length(this, name, size) result(bmi_status) bind(C, name="get_var_length")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_int), intent(out) :: size
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_name

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      f_name = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_name, size)
      deallocate(f_name)
    end function get_var_length

    ! Get a copy of values (flattened!) of the given integer variable.
    function get_value_int(this, name, dest) result(bmi_status) bind(C, name="get_value_int")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_int) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%get_value_int(f_str, dest(:num_items))
      deallocate(f_str)
    end function get_value_int

    function get_value_int1(this, name, dest) result(bmi_status) bind(C, name="get_value_int1")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=1) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%get_value_int1(f_str, dest(:num_items))
      deallocate(f_str)
    end function get_value_int1

    function get_value_int2(this, name, dest) result(bmi_status) bind(C, name="get_value_int2")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_short) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%get_value_int2(f_str, dest(:num_items))
      deallocate(f_str)
    end function get_value_int2

    function get_value_int8(this, name, dest) result(bmi_status) bind(C, name="get_value_int8")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_long) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%get_value_int8(f_str, dest(:num_items))
      deallocate(f_str)
    end function get_value_int8

    function get_value_float(this, name, dest) result(bmi_status) bind(C, name="get_value_float")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      real(kind=c_float) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%get_value_float(f_str, dest(:num_items))
      deallocate(f_str)
    end function get_value_float

    function get_value_double(this, name, dest) result(bmi_status) bind(C, name="get_value_double")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      real(kind=c_double) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%get_value_double(f_str, dest(:num_items))
      deallocate(f_str)
    end function get_value_double

    function get_value_string(this, name, dest) result(bmi_status) bind(C, name="get_value_string")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      character(kind=c_char, len=1), dimension(BMI_MAX_STRING_LENGTH), intent(inout) :: dest
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      character(kind=c_char, len=:), allocatable :: f_dest
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      allocate(character(len=num_items) :: f_dest)
      bmi_status = bmi_box%ptr%get_value_string(f_str, f_dest)
      dest(1:len_trim(f_dest)+1) = f_to_c_string(f_dest)

      deallocate(f_dest)
      deallocate(f_str)
    end function get_value_string

    function get_value_logical(this, name, dest) result(bmi_status) bind(C, name="get_value_logical")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      logical(kind=c_bool) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to get
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      logical, dimension(:), allocatable :: log4byte
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      allocate( logical::log4byte( num_items ) )
      bmi_status = bmi_box%ptr%get_value_logical(f_str, log4byte)
      ! `dest` from C is 1 byte, assign it from Fortran
      !  logical with is 4byte by default
      dest(:num_items) = log4byte
      deallocate(f_str)
      deallocate( log4byte )
    end function get_value_logical

    ! get a C pointer to the variables of integer*4 data type
    function get_value_ptr_int(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_int")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      integer, pointer :: dest_ptr(:)
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_int(f_str, dest_ptr )
      dest_cptr = c_loc( dest_ptr(1) )
      deallocate(f_str)
    end function get_value_ptr_int

    ! get a C pointer to the variables of integer*1 data type
    function get_value_ptr_int1(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_int1")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      integer(kind=1), pointer :: dest_ptr(:)
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_int1(f_str, dest_ptr )
      dest_cptr = c_loc( dest_ptr(1) )
      deallocate(f_str)
    end function get_value_ptr_int1

    ! get a C pointer to the variables of integer*2 data type
    function get_value_ptr_int2(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_int2")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      integer(kind=2), pointer :: dest_ptr(:)
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_int2(f_str, dest_ptr )
      dest_cptr = c_loc( dest_ptr(1) )
      deallocate(f_str)
    end function get_value_ptr_int2

    ! get a C pointer to the variables of integer*8 data type
    function get_value_ptr_int8(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_int8")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      integer(kind=8), pointer :: dest_ptr(:)
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_int8(f_str, dest_ptr )
      dest_cptr = c_loc( dest_ptr(1) )
      deallocate(f_str)
    end function get_value_ptr_int8

    ! get a C pointer to the variables of logical data type
    function get_value_ptr_logical(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_logical")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      !logical(kind=c_bool) :: dest_ptr(:)
      logical, dimension(:), pointer :: dest_ptr(:)
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_logical(f_str, dest_ptr )
      dest_cptr = c_loc( dest_ptr(1) )
      deallocate(f_str)
    end function get_value_ptr_logical

    ! get a C pointer to the variables of real*4 data type
    function get_value_ptr_float(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_float")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      real, pointer :: dest_ptr(:)
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_float(f_str, dest_ptr )
      dest_cptr = c_loc( dest_ptr(1) )
      deallocate(f_str)
    end function get_value_ptr_float

    ! get a C pointer to the variables of double percision data type
    function get_value_ptr_double(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_double")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      double precision, pointer :: dest_ptr(:)
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_double(f_str, dest_ptr )
      dest_cptr = c_loc( dest_ptr(1) )
      deallocate(f_str)
    end function get_value_ptr_double

    ! get a C pointer to the variables of string data type
    function get_value_ptr_string(this, name, dest_cptr) result(bmi_status) bind(C, name="get_value_ptr_string")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      type(c_ptr), intent(inout) :: dest_cptr
      integer(kind=c_int) :: bmi_status
      character(len=:), pointer :: dest_ptr
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_str
      integer :: varlength
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_value_ptr_string(f_str, dest_ptr )
      bmi_status = bmi_box%ptr%get_var_length(f_str, varlength )
      dest_ptr(varlength:varlength) = c_null_char
      dest_cptr = c_loc( dest_ptr )
      deallocate(f_str)
    end function get_value_ptr_string

    ! Set new values for an integer model variable.
    function set_value_int(this, name, src) result(bmi_status) bind(C, name="set_value_int")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_int) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
  
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%set_value_int(f_str, src(:num_items))
      deallocate(f_str)
    end function set_value_int

    function set_value_int1(this, name, src) result(bmi_status) bind(C, name="set_value_int1")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=1) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
  
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%set_value_int1(f_str, src(:num_items))
      deallocate(f_str)
    end function set_value_int1

    function set_value_int2(this, name, src) result(bmi_status) bind(C, name="set_value_int2")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_short) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
  
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%set_value_int2(f_str, src(:num_items))
      deallocate(f_str)
    end function set_value_int2

    function set_value_int8(this, name, src) result(bmi_status) bind(C, name="set_value_int8")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_long) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
  
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%set_value_int8(f_str, src(:num_items))
      deallocate(f_str)
    end function set_value_int8

    ! Set new values for a real model variable.
    function set_value_float(this, name, src) result(bmi_status) bind(C, name="set_value_float")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      real(kind=c_float) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !FIXME try both paths, nbytes/itemsize and grid info in cause some model doesn't implement
      !one one or the other????
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%set_value_float(f_str, src(:num_items))
      deallocate(f_str)
    end function set_value_float

    ! Set new values for a double model variable.
    function set_value_double(this, name, src) result(bmi_status) bind(C, name="set_value_double")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      real(kind=c_double) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
  
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%set_value_double(f_str, src(:num_items))
      deallocate(f_str)
    end function set_value_double

    function set_value_logical(this, name, src) result(bmi_status) bind(C, name="set_value_logical")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      logical(kind=c_bool) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      logical, dimension(:), allocatable :: log4byte
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
  
      f_str = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      allocate( logical::log4byte( num_items ) )
      !
      ! `src` from C is 1byte, assign it to logical with is 4byte by default
      log4byte = src(:num_items)
      write(*,*) 'bmi_iso_c:', src(:num_items)
      bmi_status = bmi_box%ptr%set_value_logical(f_str, log4byte)
      deallocate(f_str)
      deallocate(log4byte)
    end function set_value_logical

    function set_value_string(this, name, src) result(bmi_status) bind(C, name="set_value_string")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      character(kind=c_char, len=1), dimension(BMI_MAX_STRING_LENGTH), intent(in) :: src
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      !for determining the number of items to set
      integer :: item_size, num_bytes, num_items, grid
      character(kind=c_char, len=:), allocatable :: f_str
      character(kind=c_char, len=:), allocatable :: f_src
      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
  
      f_str = c_to_f_string(name)
      f_src = c_to_f_string(src)
      bmi_status = bmi_box%ptr%get_var_length(f_str, num_items)
      bmi_status = bmi_box%ptr%set_value_string(f_str, f_src)
      deallocate(f_str)
      deallocate(f_src)
    end function set_value_string

    function get_var_grid(this, name, grid) result(bmi_status) bind(C, name="get_var_grid")
      type(c_ptr), value :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_VAR_NAME), intent(in) :: name
      integer(kind=c_int), intent(out) :: grid
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      character(kind=c_char, len=:), allocatable :: f_name

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      f_name = c_to_f_string(name)
      bmi_status = bmi_box%ptr%get_var_grid(f_name, grid)
      deallocate(f_name)
    end function get_var_grid

end module iso_c_bmif_2_0
