! The Basic Model Interface (BMI) Fortran specification.
!
! This language specification is derived from the Scientific
! Interface Definition Language (SIDL) file bmi.sidl located at
! https://github.com/csdms/bmi.
!
!
! Last modified by Zhengtao Cui 
! Last modification date: Feb 18, 2022
! Description: Add functions for getting double precision pointer types.
!              Add the real*4 get value ptr function
! Last modification date: Mar 10, 2022
! Description: Cleaned up the comments.
!             
! Last modification date: Mar 31, 2022
! Description: Added *_at_indices_* functions for int1, int2, int8, logical and
!              string data types.
!
module bmif_2_0

  implicit none

  integer, parameter :: BMI_MAX_COMPONENT_NAME = 2048
  integer, parameter :: BMI_MAX_VAR_NAME = 2048
  integer, parameter :: BMI_MAX_TYPE_NAME = 2048
  integer, parameter :: BMI_MAX_UNITS_NAME = 2048
  integer, parameter :: BMI_MAX_ROLE_NAME = 2048
  integer, parameter :: BMI_MAX_LOCATION_NAME = 2048
  integer, parameter :: BMI_MAX_FILE_NAME = 2048
  integer, parameter :: BMI_MAX_STRING_LENGTH = 2048 

  integer, parameter :: BMI_FAILURE = 1
  integer, parameter :: BMI_SUCCESS = 0

  type, abstract :: bmi
    contains

      ! Initialize, run, finalize (IRF)
      procedure(bmif_initialize), deferred :: initialize
      procedure(bmif_update), deferred :: update
      procedure(bmif_update_until), deferred :: update_until
      procedure(bmif_finalize), deferred :: finalize

      ! Exchange items
      procedure(bmif_get_component_name), deferred :: get_component_name
      procedure(bmif_get_input_item_count), deferred :: get_input_item_count
      procedure(bmif_get_output_item_count), deferred :: get_output_item_count
      procedure(bmif_get_input_var_names), deferred :: get_input_var_names
      procedure(bmif_get_output_var_names), deferred :: get_output_var_names

      ! Variable information
      procedure(bmif_get_var_grid), deferred :: get_var_grid
      procedure(bmif_get_var_type), deferred :: get_var_type
      procedure(bmif_get_var_units), deferred :: get_var_units
      procedure(bmif_get_var_itemsize), deferred :: get_var_itemsize
      procedure(bmif_get_var_nbytes), deferred :: get_var_nbytes
      procedure(bmif_get_var_location), deferred :: get_var_location

      ! Time information
      procedure(bmif_get_current_time), deferred :: get_current_time
      procedure(bmif_get_start_time), deferred :: get_start_time
      procedure(bmif_get_end_time), deferred :: get_end_time
      procedure(bmif_get_time_units), deferred :: get_time_units
      procedure(bmif_get_time_step), deferred :: get_time_step

      ! Getters, by type
      procedure(bmif_get_value_int), deferred :: get_value_int
      procedure(bmif_get_value_int1), deferred :: get_value_int1
      procedure(bmif_get_value_int2), deferred :: get_value_int2
      procedure(bmif_get_value_int8), deferred :: get_value_int8
      procedure(bmif_get_value_float), deferred :: get_value_float
      procedure(bmif_get_value_double), deferred :: get_value_double
      procedure(bmif_get_value_string), deferred :: get_value_string
      procedure(bmif_get_value_logical), deferred :: get_value_logical
      procedure(bmif_get_value_ptr_int), deferred :: get_value_ptr_int
      procedure(bmif_get_value_ptr_int1), deferred :: get_value_ptr_int1
      procedure(bmif_get_value_ptr_int2), deferred :: get_value_ptr_int2
      procedure(bmif_get_value_ptr_int8), deferred :: get_value_ptr_int8
      procedure(bmif_get_value_ptr_logical), deferred :: get_value_ptr_logical
      procedure(bmif_get_value_ptr_float), deferred :: get_value_ptr_float
      procedure(bmif_get_value_ptr_double), deferred :: get_value_ptr_double
      procedure(bmif_get_value_ptr_string), deferred :: get_value_ptr_string
      
      procedure(bmif_get_value_at_indices_int), deferred :: &
           get_value_at_indices_int
      procedure(bmif_get_value_at_indices_int1), deferred :: &
           get_value_at_indices_int1
      procedure(bmif_get_value_at_indices_int2), deferred :: &
           get_value_at_indices_int2
      procedure(bmif_get_value_at_indices_int8), deferred :: &
           get_value_at_indices_int8
      procedure(bmif_get_value_at_indices_logical), deferred :: &
           get_value_at_indices_logical
      procedure(bmif_get_value_at_indices_string), deferred :: &
           get_value_at_indices_string
      procedure(bmif_get_value_at_indices_float), deferred :: &
           get_value_at_indices_float
      procedure(bmif_get_value_at_indices_double), deferred :: &
           get_value_at_indices_double

      ! Setters, by type
      procedure(bmif_set_value_int), deferred :: set_value_int
      procedure(bmif_set_value_int1), deferred :: set_value_int1
      procedure(bmif_set_value_int2), deferred :: set_value_int2
      procedure(bmif_set_value_int8), deferred :: set_value_int8
      procedure(bmif_set_value_float), deferred :: set_value_float
      procedure(bmif_set_value_double), deferred :: set_value_double
      procedure(bmif_set_value_string), deferred :: set_value_string
      procedure(bmif_set_value_logical), deferred :: set_value_logical
      procedure(bmif_set_value_at_indices_int), deferred :: &
           set_value_at_indices_int
      procedure(bmif_set_value_at_indices_int1), deferred :: &
           set_value_at_indices_int1
      procedure(bmif_set_value_at_indices_int2), deferred :: &
           set_value_at_indices_int2
      procedure(bmif_set_value_at_indices_int8), deferred :: &
           set_value_at_indices_int8
      procedure(bmif_set_value_at_indices_logical), deferred :: &
           set_value_at_indices_logical
      procedure(bmif_set_value_at_indices_string), deferred :: &
           set_value_at_indices_string
      procedure(bmif_set_value_at_indices_float), deferred :: &
           set_value_at_indices_float
      procedure(bmif_set_value_at_indices_double), deferred :: &
           set_value_at_indices_double

      ! Grid information
      procedure(bmif_get_grid_rank), deferred :: get_grid_rank
      procedure(bmif_get_grid_size), deferred :: get_grid_size
      procedure(bmif_get_grid_type), deferred :: get_grid_type

      ! Uniform rectilinear
      procedure(bmif_get_grid_shape), deferred :: get_grid_shape
      procedure(bmif_get_grid_spacing), deferred :: get_grid_spacing
      procedure(bmif_get_grid_origin), deferred :: get_grid_origin

      ! Non-uniform rectilinear, curvilinear
      procedure(bmif_get_grid_x), deferred :: get_grid_x
      procedure(bmif_get_grid_y), deferred :: get_grid_y
      procedure(bmif_get_grid_z), deferred :: get_grid_z

      ! Unstructured
      procedure(bmif_get_grid_node_count), deferred :: get_grid_node_count
      procedure(bmif_get_grid_edge_count), deferred :: get_grid_edge_count
      procedure(bmif_get_grid_face_count), deferred :: get_grid_face_count
      procedure(bmif_get_grid_edge_nodes), deferred :: get_grid_edge_nodes
      procedure(bmif_get_grid_face_edges), deferred :: get_grid_face_edges
      procedure(bmif_get_grid_face_nodes), deferred :: get_grid_face_nodes
      procedure(bmif_get_grid_nodes_per_face), deferred :: &
           get_grid_nodes_per_face
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      ! New BMI functions to support variable roles
      ! for serialization, calibration, etc.
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      procedure(bmif_get_bmi_version), deferred :: get_bmi_version
      procedure(bmif_get_var_count), deferred :: get_var_count
      procedure(bmif_get_var_names), deferred :: get_var_names
      !------------------------------------------------------------
      procedure(bmif_get_var_index), deferred :: get_var_index
      procedure(bmif_get_var_role), deferred :: get_var_role
      procedure(bmif_get_var_length), deferred :: get_var_length
  end type bmi

  abstract interface

    ! Perform startup tasks for the model.
    function bmif_initialize(this, config_file) result(bmi_status)
      import :: bmi
      class(bmi), intent(out) :: this
      character(len=*), intent(in) :: config_file
      integer :: bmi_status
    end function bmif_initialize

    ! Advance the model one time step.
    function bmif_update(this) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      integer :: bmi_status
    end function bmif_update

    ! Advance the model until the given time.
    function bmif_update_until(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      double precision, intent(in) :: time
      integer :: bmi_status
    end function bmif_update_until

    ! Perform teardown tasks for the model.
    function bmif_finalize(this) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      integer :: bmi_status
    end function bmif_finalize

    ! Get the name of the model.
    function bmif_get_component_name(this, name) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), pointer, intent(out) :: name
      integer :: bmi_status
    end function bmif_get_component_name

    ! Count a model's input variables.
    function bmif_get_input_item_count(this, count) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(out) :: count
      integer :: bmi_status
    end function bmif_get_input_item_count

    ! Count a model's output variables.
    function bmif_get_output_item_count(this, count) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(out) :: count
      integer :: bmi_status
    end function bmif_get_output_item_count

    ! List a model's input variables.
    function bmif_get_input_var_names(this, names) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function bmif_get_input_var_names

    ! List a model's output variables.
    function bmif_get_output_var_names(this, names) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function bmif_get_output_var_names

    ! Get the grid identifier for the given variable.
    function bmif_get_var_grid(this, name, grid) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: grid
      integer :: bmi_status
    end function bmif_get_var_grid

    ! Get the data type of the given variable as a string.
    function bmif_get_var_type(this, name, type) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: type
      integer :: bmi_status
    end function bmif_get_var_type

    ! Get the units of the given variable.
    function bmif_get_var_units(this, name, units) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: units
      integer :: bmi_status
    end function bmif_get_var_units

    ! Get memory use per array element, in bytes.
    function bmif_get_var_itemsize(this, name, size) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: size
      integer :: bmi_status
    end function bmif_get_var_itemsize

    ! Get size of the given variable, in bytes.
    function bmif_get_var_nbytes(this, name, nbytes) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: nbytes
      integer :: bmi_status
    end function bmif_get_var_nbytes

    ! Describe where a variable is located: node, edge, or face.
    function bmif_get_var_location(this, name, location) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: location
      integer :: bmi_status
    end function bmif_get_var_location

    ! Current time of the model.
    function bmif_get_current_time(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function bmif_get_current_time

    ! Start time of the model.
    function bmif_get_start_time(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function bmif_get_start_time

    ! End time of the model.
    function bmif_get_end_time(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function bmif_get_end_time

    ! Time units of the model.
    function bmif_get_time_units(this, units) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(out) :: units
      integer :: bmi_status
    end function bmif_get_time_units

    ! Time step of the model.
    function bmif_get_time_step(this, time_step) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time_step
      integer :: bmi_status
    end function bmif_get_time_step

    ! Get a copy of values (flattened!) of the given integer variable.
    function bmif_get_value_int(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_int

    function bmif_get_value_int1(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(kind=1), intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_int1

    function bmif_get_value_int2(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(kind=2), intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_int2

    function bmif_get_value_int8(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(kind=8), intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_int8

    ! Get a copy of values (flattened!) of the given real variable.
    function bmif_get_value_float(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_float

    ! Get a copy of values (flattened!) of the given double variable.
    function bmif_get_value_double(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_double

    function bmif_get_value_string(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(inout) :: dest
      integer :: bmi_status
    end function bmif_get_value_string

    function bmif_get_value_logical(this, name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      logical, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_logical

    ! Get a reference to the given integer variable.
    function bmif_get_value_ptr_int(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      integer, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_int

    function bmif_get_value_ptr_int1(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      integer(kind=1), pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_int1

    function bmif_get_value_ptr_int2(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      integer(kind=2), pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_int2

    function bmif_get_value_ptr_int8(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      integer(kind=8), pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_int8

    function bmif_get_value_ptr_logical(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      logical, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_logical

    ! Get a reference to the given real variable.
    function bmif_get_value_ptr_float(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      !
      ! Here the `target` attribute for `this` is necessary, so that pointers 
      ! can be used to point to components of the bmi object, not only pointer
      !  type, but also other types such as scalar, alloctable array, and 
      ! fixed size static array.
      ! 
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      real, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_float

    ! Get a reference to the given double variable.
    function bmif_get_value_ptr_double(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      !
      ! Here the `target` attribute for `this` is necessary, so that pointers 
      ! can be used to point to components of the bmi object, not only pointer
      !  type, but also other types such as scalar, alloctable array, and 
      ! fixed size static array.
      ! 
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      double precision, pointer, intent(inout) :: dest_ptr(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_double

    ! Get a reference to the given string variable.
    function bmif_get_value_ptr_string(this, name, dest_ptr) result(bmi_status)
      import :: bmi
      !
      ! Here the `target` attribute for `this` is necessary, so that pointers 
      ! can be used to point to components of the bmi object, not only pointer
      !  type, but also other types such as scalar, alloctable array, and 
      ! fixed size static array.
      ! 
      class(bmi), intent(in), target :: this
      character(len=*), intent(in) :: name
      character(len=:), pointer, intent(inout) :: dest_ptr
      integer :: bmi_status
    end function bmif_get_value_ptr_string

    ! Get integer values at particular (one-dimensional) indices.
    function bmif_get_value_at_indices_int(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_int

    function bmif_get_value_at_indices_int1(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(kind=1), intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_int1

    function bmif_get_value_at_indices_int2(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(kind=2), intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_int2

    function bmif_get_value_at_indices_int8(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer(kind=8), intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_int8

    function bmif_get_value_at_indices_logical(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      logical, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_logical

    function bmif_get_value_at_indices_string(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(inout) :: dest
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_string

    ! Get real values at particular (one-dimensional) indices.
    function bmif_get_value_at_indices_float(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_float

    ! Get double values at particular (one-dimensional) indices.
    function bmif_get_value_at_indices_double(this, name, dest, inds) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      double precision, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_double

    ! Set new values for an integer model variable.
    function bmif_set_value_int(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_int

    function bmif_set_value_int1(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer(kind=1), intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_int1

    function bmif_set_value_int2(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer(kind=2), intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_int2

    function bmif_set_value_int8(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer(kind=8), intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_int8

    ! Set new values for a real model variable.
    function bmif_set_value_float(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_float

    ! Set new values for a double model variable.
    function bmif_set_value_double(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_double

    function bmif_set_value_string(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: src
      integer :: bmi_status
    end function bmif_set_value_string

    function bmif_set_value_logical(this, name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: name
      logical, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_logical

    ! Set integer values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_int(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_int

    ! Set integer values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_int1(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      integer(kind=1), intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_int1

    ! Set integer values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_int2(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      integer(kind=2), intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_int2

    ! Set integer values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_int8(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      integer(kind=8), intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_int8

    ! Set logical values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_logical(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      logical, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_logical

    ! Set logical values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_string(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      character(len=*), intent(in) :: src
      integer :: bmi_status
    end function bmif_set_value_at_indices_string

    ! Set real values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_float(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_float

    ! Set double values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_double(this, name, inds, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout), target :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_double

    ! Get number of dimensions of the computational grid.
    function bmif_get_grid_rank(this, grid, rank) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: rank
      integer :: bmi_status
    end function bmif_get_grid_rank

    ! Get the total number of elements in the computational grid.
    function bmif_get_grid_size(this, grid, size) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: size
      integer :: bmi_status
    end function bmif_get_grid_size

    ! Get the grid type as a string.
    function bmif_get_grid_type(this, grid, type) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      character(len=*), intent(out) :: type
      integer :: bmi_status
    end function bmif_get_grid_type

    ! Get the dimensions of the computational grid.
    function bmif_get_grid_shape(this, grid, shape) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: shape
      integer :: bmi_status
    end function bmif_get_grid_shape

    ! Get distance between nodes of the computational grid.
    function bmif_get_grid_spacing(this, grid, spacing) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: spacing
      integer :: bmi_status
    end function bmif_get_grid_spacing

    ! Get coordinates of the origin of the computational grid.
    function bmif_get_grid_origin(this, grid, origin) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: origin
      integer :: bmi_status
    end function bmif_get_grid_origin

    ! Get the x-coordinates of the nodes of a computational grid.
    function bmif_get_grid_x(this, grid, x) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: x
      integer :: bmi_status
    end function bmif_get_grid_x

    ! Get the y-coordinates of the nodes of a computational grid.
    function bmif_get_grid_y(this, grid, y) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: y
      integer :: bmi_status
    end function bmif_get_grid_y

    ! Get the z-coordinates of the nodes of a computational grid.
    function bmif_get_grid_z(this, grid, z) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: z
      integer :: bmi_status
    end function bmif_get_grid_z

    ! Get the number of nodes in an unstructured grid.
    function bmif_get_grid_node_count(this, grid, count) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function bmif_get_grid_node_count

    ! Get the number of edges in an unstructured grid.
    function bmif_get_grid_edge_count(this, grid, count) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function bmif_get_grid_edge_count

    ! Get the number of faces in an unstructured grid.
    function bmif_get_grid_face_count(this, grid, count) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status
    end function bmif_get_grid_face_count

    ! Get the edge-node connectivity.
    function bmif_get_grid_edge_nodes(this, grid, edge_nodes) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: edge_nodes
      integer :: bmi_status
    end function bmif_get_grid_edge_nodes

    ! Get the face-edge connectivity.
    function bmif_get_grid_face_edges(this, grid, face_edges) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_edges
      integer :: bmi_status
    end function bmif_get_grid_face_edges

    ! Get the face-node connectivity.
    function bmif_get_grid_face_nodes(this, grid, face_nodes) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_nodes
      integer :: bmi_status
    end function bmif_get_grid_face_nodes

    ! Get the number of nodes for each face.
    function bmif_get_grid_nodes_per_face(this, grid, nodes_per_face) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: nodes_per_face
      integer :: bmi_status
    end function bmif_get_grid_nodes_per_face

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      ! New BMI functions to support variable roles
      ! for serialization, calibration, etc.
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    function bmif_get_bmi_version(this, version) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(out) :: version
      integer :: bmi_status
    end function bmif_get_bmi_version
    ! Count a model's variables.
    function bmif_get_var_count(this, role, count) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: role
      integer, intent(out) :: count
      integer :: bmi_status
    end function bmif_get_var_count
    ! List a model's variables.
    function bmif_get_var_names(this, role, names) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: role
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function bmif_get_var_names

    function bmif_get_var_index(this, name, index) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: index
      integer :: bmi_status
    end function bmif_get_var_index

    function bmif_get_var_role(this, name, role) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: role
      integer :: bmi_status
    end function bmif_get_var_role

    function bmif_get_var_length(this, name, size) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: size
      integer :: bmi_status
    end function bmif_get_var_length
  end interface

end module bmif_2_0
