#ifndef ISO_C_BMIF_H
#define ISO_C_BMIF_H

#include <stdbool.h>

extern int bmi_factory(void **);
extern int bmi_destroy(void **);

extern int c_create_box(void **, void **);
extern int c_delete_box(void **);

extern int initialize(void **, const char*);
extern int get_component_name(void **, char*);
extern int get_start_time(void **, double* );
extern int get_end_time(void **, double* );
extern int get_current_time(void **, double* );
extern int update(void **);
extern int finalize(void **);

extern int get_var_count(void **, char*,  int* );
extern int get_var_type(void **, char*,  char* );
extern int get_var_names(void **, char*,  char** );

extern int get_var_length(void **, char*,  int* );
extern int get_value_int(void **, char*,  int* );
extern int get_value_int1(void **, char*,  char* );
extern int get_value_int2(void **, char*,  short* );
extern int get_value_int8(void **, char*,  long* );
extern int get_value_float(void **, char*,  float* );
extern int get_value_double(void **, char*,  double* );
extern int get_value_string(void **, char*,  char* );
extern int get_value_logical(void **, char*,  bool* );
extern int get_var_grid(void **, char*,  int* );

extern int get_value_ptr_double_scalar(void **, char*,  double** );
extern int get_value_ptr_double_1darray(void **, char*,  double** );
extern int get_value_ptr_double_2darray(void **, char*,  double** );

extern int set_value_int(void **, char*,  int* );
extern int set_value_int1(void **, char*,  char* );
extern int set_value_int2(void **, char*,  short* );
extern int set_value_int8(void **, char*,  long* );
extern int set_value_float(void **, char*,  float* );
extern int set_value_double(void **, char*,  double* );
extern int set_value_string(void **, char*,  char* );
extern int set_value_logical(void **, char*,  bool* );
#endif // #ifndef ISO_C_BMIF_H
