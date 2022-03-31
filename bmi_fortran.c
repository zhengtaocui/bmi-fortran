/** ----------------------------------------------
  * bmi_fortran.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Mar. 4, 2022
  * Last date of modification: Mar 7, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: These are the wrapper functions and implements the C BMI 
  *              struct to be used by the cfe serialization code.
  *              
  * Last modified by Zhengtao Cui (Zhengtao.Cui@noaa.gov)
  * Last date of modification: Mar 10, 2022
  * Description: Updated Get_var_names function because the Fortran 
  *             get_var_names function now resturn a copy of the names.
  *
  * Last modified on Mar 18, 2022
  * Description: renamed the register_bmi_fortran function.
  *
  * Last modified on Mar 31, 2022
  * Description: added missing BMI functions
  */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "iso_c_bmif_2_0.h"
#include "bmi_fortran.h"
#include "bmi.h"

char* trim(char* str) {
   char * start = str;
   char * end = start + strlen(str);

   while (--end >= start) {   /* trim right */
      if (!isspace(*end))
         break;
   }
   *(++end) = '\0';

   while (isspace(*start))    /* trim left */
      start++;

   if (start != str)          /* there is a string */
      memmove(str, start, end - start + 1);
}

int Initialize(Bmi *self, const char* file )
{
   int bmi_status = initialize( self->data, file );
   return bmi_status;   
}

int Get_var_count (Bmi *self, const char *role, int *count)
{
   int bmi_status = get_var_count( self->data, role, count );
   return bmi_status;   
}

int Get_var_names (Bmi *self, const char *role, char **names)
{
//   int count;
   int bmi_status = get_var_names( self->data, role, names );
   /*
    * get_var_names is changed to copy the content to the
    * string array of names. The following is not needed.
    */
//   char **fornames = (char**)NULL;
//   fornames = (char**)malloc( sizeof(char*)); 
//   int bmi_status = get_var_names( self->data, role, fornames );
//   bmi_status = get_var_count( self->data, role, &count );
//   for ( int i = 0; i < count; ++i )
//   {
//      //populate the array of C stings.
//      strncpy(names[i], fornames[0] + i * BMI_MAX_VAR_NAME, BMI_MAX_VAR_NAME ); 
//      names[i] = trim( names[i] );
//   }
//   free(fornames);
   return bmi_status;   
}

int Get_var_length (Bmi *self, const char *name, int *size)
{
   int bmi_status = get_var_length( self->data, name, size );
   return bmi_status;   
}

int Get_var_type (Bmi *self, const char *name, char *type)
{
   int bmi_status = get_var_type( self->data, name, type );
   return bmi_status;   
}

int Get_value_ptr (Bmi *self, const char *name, void **ptr)
{
    if (!self){
        return BMI_FAILURE;
    }
    char type[BMI_MAX_TYPE_NAME];

    int bmi_status = get_var_type( self->data, name, type );

    if ( strcmp(type, "integer1" ) == 0 )
    {
        bmi_status = get_value_ptr_int1( self->data, name, (int8_t**)ptr );
    }
    else if ( strcmp(type, "integer2" ) == 0 )
    {
        bmi_status = get_value_ptr_int2( self->data, name, (short**)ptr );
    }
    else if ( strcmp(type, "integer4" ) == 0 )
    {
        bmi_status = get_value_ptr_int( self->data, name, (int**)ptr );
    }
    else if ( strcmp(type, "integer8" ) == 0 )
    {
        bmi_status = get_value_ptr_int8( self->data, name, (long**)ptr );
    }
    else if ( strcmp(type, "real4" ) == 0 )
    {
        bmi_status = get_value_ptr_float( self->data, name, (float**)ptr );
    }
    else if ( strcmp(type, "real8" ) == 0 )
    {
        bmi_status = get_value_ptr_double( self->data, name, (double**)ptr );
    }
    else if ( strcmp(type, "logical" ) == 0 )
    {
        /*
	 * Fortran logical type by default is 4 byte
	 */
        bmi_status = get_value_ptr_logical( self->data, name, (int**)ptr );
    }
    else if ( strcmp(type, "character" ) == 0 )
    {
        bmi_status = get_value_ptr_string( self->data, name, (char**)ptr );
    }
    else
    {
        bmi_status = BMI_FAILURE;
    }

    return bmi_status;
}

int Get_value (Bmi *self, const char *name, void *dest)
{
    if (!self){
        return BMI_FAILURE;
    }
    char type[BMI_MAX_TYPE_NAME];

    int bmi_status = get_var_type( self->data, name, type );

    if ( strcmp(type, "integer1" ) == 0 )
    {
        bmi_status = get_value_int1( self->data, name, (int8_t*)dest );
    }
    else if ( strcmp(type, "integer2" ) == 0 )
    {
        bmi_status = get_value_int2( self->data, name, (short*)dest );
    }
    else if ( strcmp(type, "integer4" ) == 0 )
    {
        bmi_status = get_value_int( self->data, name, (int*)dest );
    }
    else if ( strcmp(type, "integer8" ) == 0 )
    {
        bmi_status = get_value_int8( self->data, name, (long*)dest );
    }
    else if ( strcmp(type, "real4" ) == 0 )
    {
        bmi_status = get_value_float( self->data, name, (float*)dest );
    }
    else if ( strcmp(type, "real8" ) == 0 )
    {
        bmi_status = get_value_double( self->data, name, (double*)dest );
    }
    else if ( strcmp(type, "logical" ) == 0 )
    {
        bmi_status = get_value_logical( self->data, name, (bool*)dest );
    }
    else if ( strcmp(type, "character" ) == 0 )
    {
        bmi_status = get_value_string( self->data, name, (char*)dest );
    }
    else
    {
        bmi_status = BMI_FAILURE;
    }

    return bmi_status;
}

int Set_value (Bmi *self, const char *name, void *src_ptr )
{
    if (!self){
        return BMI_FAILURE;
    }
    char type[BMI_MAX_TYPE_NAME];

    int bmi_status = get_var_type( self->data, name, type );

    if ( strcmp(type, "integer1" ) == 0 )
    {
        bmi_status = set_value_int1( self->data, name, (int8_t*)src_ptr );
    }
    else if ( strcmp(type, "integer2" ) == 0 )
    {
        bmi_status = set_value_int2( self->data, name, (short*)src_ptr );
    }
    else if ( strcmp(type, "integer4" ) == 0 )
    {
        bmi_status = set_value_int( self->data, name, (int*)src_ptr );
    }
    else if ( strcmp(type, "integer8" ) == 0 )
    {
        bmi_status = set_value_int8( self->data, name, (long*)src_ptr );
    }
    else if ( strcmp(type, "real4" ) == 0 )
    {
        bmi_status = set_value_float( self->data, name, (float*)src_ptr );
    }
    else if ( strcmp(type, "real8" ) == 0 )
    {
        bmi_status = set_value_double( self->data, name, (double*)src_ptr );
    }
    else if ( strcmp(type, "logical" ) == 0 )
    {
        bmi_status = set_value_logical( self->data, name, (bool*)src_ptr );
    }
    else if ( strcmp(type, "character" ) == 0 )
    {
        bmi_status = set_value_string( self->data, name, (char*)src_ptr );
    }
    else
    {
        bmi_status = BMI_FAILURE;
    }

    return bmi_status;
}

int Update (Bmi *self)
{
   int bmi_status = update( self->data );
   return bmi_status;   
}

int Update_until (Bmi *self, double then)
{
   int bmi_status = update_until( self->data, then );
   return bmi_status;   
}

int Get_component_name (Bmi *self, char * name)
{
   int bmi_status = get_component_name( self->data, name );
   return bmi_status;   
}

int Get_start_time (Bmi *self, double * time)
{
   int bmi_status = get_start_time( self->data, time );
   return bmi_status;   
}

int Get_end_time (Bmi *self, double * time)
{
   int bmi_status = get_end_time( self->data, time );
   return bmi_status;   
}

int Get_current_time (Bmi *self, double * time)
{
   int bmi_status = get_current_time( self->data, time );
   return bmi_status;   
}

int Finalize (Bmi *self)
{
   int bmi_status = finalize( self->data );
   return bmi_status;   
}

int Get_input_item_count (Bmi *self, int *count)
{
    int result;
    char *role = "input_from_bmi";
    //char role[BMI_MAX_ROLE_NAME] = "input_from_bmi";
    
    result = Get_var_count( self, role, count );

    if (result == 0){
        return BMI_SUCCESS;
    } else{
        return BMI_FAILURE;
    }
}

int Get_output_item_count (Bmi *self, int *count)
{
    int result;
    char *role = "output_to_bmi";
    //char role[BMI_MAX_ROLE_NAME] = "output_to_bmi";
    
    result = Get_var_count( self, role, count );

    if (result == 0){
        return BMI_SUCCESS;
    } else{
        return BMI_FAILURE;
    }
    
    // *count = OUTPUT_VAR_NAME_COUNT;
    // return BMI_SUCCESS;
}

int Get_input_var_names (Bmi *self, char **names)
{
    int result;
    char *role = "input_from_bmi";
    //char role[BMI_MAX_ROLE_NAME] = "input_from_bmi";
    
    result = Get_var_names( self, role, names );

    if (result == 0){
        return BMI_SUCCESS;
    } else{
        return BMI_FAILURE;
    }
}

int Get_output_var_names (Bmi *self, char **names)
{
    int result;
    char *role = "output_to_bmi";
    //char role[BMI_MAX_ROLE_NAME] = "output_to_bmi";
    
    result = Get_var_names( self, role, names );

    if (result == 0){
        return BMI_SUCCESS;
    } else{
        return BMI_FAILURE;
    }
}

int Get_var_grid(Bmi *self, const char *name, int *grid)
{
    //-------------------------------------------------
    // Note: This pulls information from the var_info
    //-------------------------------------------------
    //
    int status = BMI_SUCCESS;
    if (!self){
        return BMI_FAILURE;   
    }

    status =  get_var_grid( self, name, grid );

    //--------------------------
    // No match found for name
    //--------------------------
    if ( status == BMI_FAILURE )
    {
      printf("ERROR in get_var_grid():\n");
      printf("  No match for: %s\n\n", name);
      *grid = -1;
    } 
    return status;
}

static int Get_var_units (Bmi *self, const char *name, char *units)
{
    //-------------------------------------------------
    // Note: This pulls information from the var_info
    // structure defined at the top, which helps to 
    // prevent implementation errors.   
    //-------------------------------------------------
    if (!self){
        return BMI_FAILURE;   
    }

    int status = get_var_units( self, name, units );

    return status;
}

int Get_var_itemsize (Bmi *self, const char *name, int *size)
{
    int status = BMI_SUCCESS;
    if (!self){
        return BMI_FAILURE;   
    }

    status =  get_var_itemsize( self, name, size );

    if ( status == BMI_FAILURE )
    {
      printf("ERROR in get_var_itemsize():\n");
      printf("  No match for: %s\n\n", name);
      *size = -1;
    } 
    return status;
}

int Get_var_nbytes (Bmi *self, const char *name, int *nbytes)
{
    int status = get_var_nbytes( self, name, nbytes );
    
    return status;
}

int Get_var_location (Bmi *self, const char *name, char *location)
{

    int status = get_var_location( self, name, location );

    if ( status == BMI_FAILURE )
    {
       //--------------------------
       // No match found for name
       //--------------------------
       printf("ERROR in get_var_location():\n");
       printf("  No match for: %s\n\n", name);
       location = "unknown";
    }
    return status;
}

int Get_time_units (Bmi *self, char * units)
{
    int status = get_time_units( self, units );
    return status;
}

int Get_time_step (Bmi *self, double * dt)
{
    int status = get_time_step( self, dt );
    return BMI_SUCCESS;
}

int Get_value_at_indices (Bmi *self, const char *name, void *dest, int *inds, int len)
{
    if (!self){
        return BMI_FAILURE;
    }
    char type[BMI_MAX_TYPE_NAME];

    int bmi_status = get_var_type( self->data, name, type );

    if ( strcmp(type, "integer1" ) == 0 )
    {
        bmi_status = get_value_at_indices_int1( 
			self->data, name, (int8_t*)dest, inds, len );
    }
    else if ( strcmp(type, "integer2" ) == 0 )
    {
        bmi_status = get_value_at_indices_int2( 
			self->data, name, (short*)dest, inds, len );
    }
    else if ( strcmp(type, "integer4" ) == 0 )
    {
        bmi_status = get_value_at_indices_int( 
			self->data, name, (int*)dest, inds, len );
    }
    else if ( strcmp(type, "integer8" ) == 0 )
    {
        bmi_status = get_value_at_indices_int8(
		        self->data, name, (long*)dest, inds, len );
    }
    else if ( strcmp(type, "real4" ) == 0 )
    {
        bmi_status = get_value_at_indices_float( 
			self->data, name, (float*)dest, inds, len );
    }
    else if ( strcmp(type, "real8" ) == 0 )
    {
        bmi_status = get_value_at_indices_double( 
			self->data, name, (double*)dest, inds, len );
    }
    else if ( strcmp(type, "logical" ) == 0 )
    {
        bmi_status = get_value_at_indices_logical( 
			self->data, name, (bool*)dest, inds, len );
    }
    else if ( strcmp(type, "character" ) == 0 )
    {
        bmi_status = get_value_at_indices_string( 
			self->data, name, (char*)dest, inds, len );
    }
    else
    {
        bmi_status = BMI_FAILURE;
    }

    return bmi_status;
}

int Set_value_at_indices (Bmi *self, const char *name, int * inds, int len, void *src)
{
    if (!self){
        return BMI_FAILURE;
    }
    if (len < 1)
        return BMI_FAILURE;

    char type[BMI_MAX_TYPE_NAME];

    int bmi_status = get_var_type( self->data, name, type );

    if ( strcmp(type, "integer1" ) == 0 )
    {
        bmi_status = set_value_at_indices_int1( 
			self->data, name, inds, (int8_t*)src, len );
    }
    else if ( strcmp(type, "integer2" ) == 0 )
    {
        bmi_status = set_value_at_indices_int2( 
			self->data, name, inds, (short*)src, len );
    }
    else if ( strcmp(type, "integer4" ) == 0 )
    {
        bmi_status = set_value_at_indices_int( 
			self->data, name, inds, (int*)src, len );
    }
    else if ( strcmp(type, "integer8" ) == 0 )
    {
        bmi_status = set_value_at_indices_int8(
			self->data, name, inds, (long*)src, len );
    }
    else if ( strcmp(type, "real4" ) == 0 )
    {
        bmi_status = set_value_at_indices_float( 
			self->data, name, inds, (float*)src, len );
    }
    else if ( strcmp(type, "real8" ) == 0 )
    {
        bmi_status = set_value_at_indices_double( 
			self->data, name, inds, (double*)src, len );
    }
    else if ( strcmp(type, "logical" ) == 0 )
    {
        bmi_status = set_value_at_indices_logical( 
			self->data, name, inds, (bool*)src, len );
    }
    else if ( strcmp(type, "character" ) == 0 )
    {
        bmi_status = set_value_at_indices_string( 
			self->data, name, inds, (char*)src, len );
    }
    else
    {
        bmi_status = BMI_FAILURE;
    }

    return bmi_status;
}

//#################################################
//  New BMI functions to support variable roles
//  for serialization, calibration, etc.
//#################################################
int Get_bmi_version (Bmi *self, char *version)
{
   int bmi_status = get_bmi_version( self->data, version );
   return bmi_status;   
}

int Get_var_index (Bmi *self, const char *name, int *index)
{
    if (!self){
        return BMI_FAILURE;   
    }

    int status = get_var_index( self->data, name, index );

    if ( status == BMI_FAILURE )
    {
      //--------------------------
      // No match found for name
      //--------------------------
      printf("ERROR in get_var_index():\n");
      printf("  No match for: %s\n\n", name);
      *index = -1;
      return BMI_FAILURE;
    }
    return status;
}

int Get_var_role (Bmi *self, const char *name, char *role)
{
    //-------------------------------------------------
    // Note: This pulls information from the var_info
    // structure defined at the top, which helps to 
    // prevent implementation errors.   
    //-------------------------------------------------
    if (!self){
        return BMI_FAILURE;   
    }
    int status = get_var_role( self->data, name, role ); 

    if ( status == BMI_FAILURE )
    {
      //--------------------------
      // No match found for name
      //--------------------------
      printf("ERROR in get_var_role():\n");
      printf("  No match for: %s\n\n", name); 
      role = "not_set";
      return BMI_FAILURE;
    }
    return status;
}

int Get_grid_size(Bmi *self, int grid, int * size)
{
      int status = get_grid_size( self->data, &grid, size );
      return status;
}

int Get_grid_rank(Bmi *self, int grid, int * rank)
{
      int status = get_grid_rank( self->data, &grid, rank );
      return status;
}

int Get_grid_type(Bmi *self, int grid, char * type)
{
      int status = get_grid_type( self->data, &grid, type  );
      return status;
}

int Get_grid_shape(Bmi *self, int grid, int *shape)
{
    return get_grid_shape( self->data, &grid, shape );
}


int Get_grid_spacing(Bmi *self, int grid, double *spacing)
{
    return get_grid_spacing( self->data, &grid, spacing );
}


int Get_grid_origin(Bmi *self, int grid, double *origin)
{
    return get_grid_origin( self->data, &grid, origin );
}

//-----------------------------------------------------------------------
/* Non-uniform rectilinear, curvilinear (grid type)*/
int Get_grid_x(Bmi *self, int grid, double *x)
{
    return get_grid_x( self->data, &grid, x );
}

int Get_grid_y(Bmi *self, int grid, double *y)
{
    return get_grid_y( self->data, &grid, y );
}

int Get_grid_z(Bmi *self, int grid, double *z)
{
    return get_grid_z( self->data, &grid, z );
}

//-----------------------------------------------------------------------
/*Unstructured (grid type)*/
int Get_grid_node_count(Bmi *self, int grid, int *count)
{
    return get_grid_node_count( self->data, &grid, count );
}


int Get_grid_edge_count(Bmi *self, int grid, int *count)
{
    return get_grid_edge_count( self->data, &grid, count );
}


int Get_grid_face_count(Bmi *self, int grid, int *count)
{
    return get_grid_face_count( self->data, &grid, count );
}


int Get_grid_edge_nodes(Bmi *self, int grid, int *edge_nodes)
{
    return get_grid_edge_nodes( self->data, &grid, edge_nodes );
}


int Get_grid_face_edges(Bmi *self, int grid, int *face_edges)
{
    return get_grid_face_edges( self->data, &grid, face_edges );
}


int Get_grid_face_nodes(Bmi *self, int grid, int *face_nodes)
{
    return get_grid_face_nodes( self->data, &grid, face_nodes );
}


int Get_grid_nodes_per_face(Bmi *self, int grid, int *nodes_per_face)
{
    return get_grid_nodes_per_face( self->data, &grid, nodes_per_face );
}

Bmi* create_bmi_fortran_model_handle(Bmi *model, void* box_handle )
{
    if (model) {
        model->data = box_handle;
        model->initialize = Initialize;
        model->update = Update;
        model->update_until = Update_until;
        model->finalize = Finalize;

        model->get_component_name = Get_component_name;
        model->get_input_item_count = Get_input_item_count;
        model->get_output_item_count = Get_output_item_count;
        model->get_input_var_names = Get_input_var_names;
        model->get_output_var_names = Get_output_var_names;

        model->get_var_grid = Get_var_grid;
        model->get_var_type = Get_var_type;
        model->get_var_itemsize = Get_var_itemsize;
        model->get_var_units = Get_var_units;
        model->get_var_nbytes = Get_var_nbytes;
        model->get_var_location = Get_var_location;

        model->get_current_time = Get_current_time;
        model->get_start_time   = Get_start_time;
        model->get_end_time     = Get_end_time;
        model->get_time_units   = Get_time_units;
        model->get_time_step    = Get_time_step;

        model->get_value = Get_value;
        model->get_value_ptr = Get_value_ptr;
        model->get_value_at_indices = Get_value_at_indices;
        model->set_value            = Set_value;  // generalized
        model->set_value_at_indices = Set_value_at_indices;

        model->get_bmi_version = Get_bmi_version;
        model->get_var_count = Get_var_count;
        model->get_var_names = Get_var_names;
        model->get_var_index = Get_var_index;
        model->get_var_role    = Get_var_role;
        model->get_var_length = Get_var_length;

        model->get_grid_size = Get_grid_size;    
        model->get_grid_rank = Get_grid_rank;    
        model->get_grid_type = Get_grid_type;    

        model->get_grid_shape   = Get_grid_shape; 
        model->get_grid_spacing = Get_grid_spacing;
        model->get_grid_origin  = Get_grid_origin;

        model->get_grid_x = Get_grid_x;    // N/a for grid type scalar
        model->get_grid_y = Get_grid_y;    // N/a for grid type scalar
        model->get_grid_z = Get_grid_z;    // N/a for grid type scalar

        model->get_grid_node_count = Get_grid_node_count;    // N/a for grid type scalar
        model->get_grid_edge_count = Get_grid_edge_count;    // N/a for grid type scalar
        model->get_grid_face_count = Get_grid_face_count;    // N/a for grid type scalar
        model->get_grid_edge_nodes = Get_grid_edge_nodes;    // N/a for grid type scalar
        model->get_grid_face_edges = Get_grid_face_edges;    // N/a for grid type scalar
        model->get_grid_face_nodes = Get_grid_face_nodes;    // N/a for grid type scalar
        model->get_grid_nodes_per_face = Get_grid_nodes_per_face;    // N/a for grid type scalar
    }
    return model;
}
