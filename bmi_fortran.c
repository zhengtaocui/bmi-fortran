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
        bmi_status = get_value_ptr_int1( self->data, name, (char**)ptr );
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
        bmi_status = get_value_int1( self->data, name, (char*)dest );
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
        bmi_status = set_value_int1( self->data, name, (char*)src_ptr );
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

Bmi* create_bmi_fortran_model_handle(Bmi *model, void* box_handle )
{
    if (model) {
        model->data = box_handle;
        model->initialize = Initialize;
        model->update = Update;
        model->finalize = Finalize;
        model->get_current_time = Get_current_time;
        model->get_start_time   = Get_start_time;
        model->get_end_time     = Get_end_time;
        model->get_component_name = Get_component_name;
        model->get_var_count = Get_var_count;
        model->get_var_names = Get_var_names;
        model->get_var_length = Get_var_length;
        model->get_var_type = Get_var_type;
        model->get_value_ptr = Get_value_ptr;
        model->get_value = Get_value;
        model->set_value = Set_value;
    }
    return model;
}
