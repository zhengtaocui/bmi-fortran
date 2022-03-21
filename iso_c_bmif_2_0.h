#ifndef ISO_C_BMIF_H
#define ISO_C_BMIF_H

#include <stdbool.h>

/*
 * These register functions are not implemented in the iso_c_bmif_2_0.f90.
 * They should be implmented in the concrete type of BMI, such as in the
 * bmi_sine.f90 file. 
 */
extern int get_bmi_handle(void **);
extern int destroy_bmi_handle(void **);

extern int get_box_handle(void **, void **);
extern int destroy_box_handle(void **);

extern int register_bmi(void **);
extern int unregister_bmi(void **);
/*
 * -------------------------------------------
 */

extern int initialize(void *, const char*);
extern int get_component_name(void *, char*);
extern int get_start_time(void *, double* );
extern int get_end_time(void *, double* );
extern int get_current_time(void *, double* );
extern int update(void *);
extern int finalize(void *);

extern int get_var_count(void *, const char*,  int* );
extern int get_var_type(void *, const char*,  char* );
extern int get_var_names(void *, const char*,  char** );

extern int get_var_length(void *,const char*,  int* );
extern int get_value_int(void *, const char*,  int* );
extern int get_value_int1(void *,const  char*,  char* );
extern int get_value_int2(void *,const  char*,  short* );
extern int get_value_int8(void *,const  char*,  long* );
extern int get_value_float(void *,const  char*,  float* );
extern int get_value_double(void *,const  char*,  double* );
extern int get_value_string(void *,const  char*,  char* );
extern int get_value_logical(void *,const  char*,  bool* );
extern int get_var_grid(void *,const  char*,  int* );

extern int get_value_ptr_int(void *,const  char*,  int** );
extern int get_value_ptr_int1(void *,const  char*,  char** );
extern int get_value_ptr_int2(void *,const  char*,  short** );
extern int get_value_ptr_int8(void *,const  char*,  long** );
extern int get_value_ptr_logical(void *,const  char*,  int** );
extern int get_value_ptr_string(void *,const  char*,  char** );

extern int get_value_ptr_float(void *,const  char*,  float** );
extern int get_value_ptr_double(void *,const  char*,  double** );
/*
extern int get_value_ptr_double_scalar(void **, char*,  double** );
extern int get_value_ptr_double_1darray(void **, char*,  double** );
extern int get_value_ptr_double_2darray(void **, char*,  double** );
*/

extern int set_value_int(void *,const  char*,  int* );
extern int set_value_int1(void *,const  char*,  char* );
extern int set_value_int2(void *,const  char*,  short* );
extern int set_value_int8(void *,const  char*,  long* );
extern int set_value_float(void *,const  char*,  float* );
extern int set_value_double(void *,const  char*,  double* );
extern int set_value_string(void *,const  char*,  char* );
extern int set_value_logical(void *,const  char*,  bool* );
#endif // #ifndef ISO_C_BMIF_H
