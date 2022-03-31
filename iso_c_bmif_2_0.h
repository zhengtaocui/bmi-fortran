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
extern int get_bmi_version(void *, char*);
extern int get_start_time(void *, double* );
extern int get_end_time(void *, double* );
extern int get_current_time(void *, double* );
extern int get_time_units(void *, char* );
extern int get_time_step(void *, double* );
extern int update(void *);
extern int update_until(void *, double );
extern int finalize(void *);

extern int get_var_count(void *, const char*,  int* );
extern int get_var_index(void *, const char*,  int* );
extern int get_var_type(void *, const char*,  char* );
extern int get_var_role(void *, const char*,  char* );
extern int get_var_units(void *, const char*,  char* );
extern int get_var_itemsize(void *, const char*, int* );
extern int get_var_nbytes(void *, const char*, int* );
extern int get_var_location(void *, const char*, char* );
extern int get_var_names(void *, const char*,  char** );

extern int get_var_length(void *,const char*,  int* );
extern int get_value_int(void *, const char*,  int* );
extern int get_value_int1(void *,const  char*,  int8_t* );
extern int get_value_int2(void *,const  char*,  short* );
extern int get_value_int8(void *,const  char*,  long* );
extern int get_value_float(void *,const  char*,  float* );
extern int get_value_double(void *,const  char*,  double* );
extern int get_value_string(void *,const  char*,  char* );
extern int get_value_logical(void *,const  char*,  bool* );
extern int get_var_grid(void *,const  char*,  int* );

extern int get_value_ptr_int(void *,const  char*,  int** );
extern int get_value_ptr_int1(void *,const  char*,  int8_t** );
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

extern int get_value_at_indices_int(void *,const  char*,  int*, int*, int );
extern int get_value_at_indices_int1(void *,const  char*,  int8_t*, int*, int );
extern int get_value_at_indices_int2(void *,const  char*,  short*, int*, int );
extern int get_value_at_indices_int8(void *,const  char*,  long*, int*, int );
extern int get_value_at_indices_logical(void *,const  char*,  bool*, int*, int );
extern int get_value_at_indices_string(void *,const  char*,  char*, int*, int );
extern int get_value_at_indices_float(void *,const  char*,  float*, int*, int );
extern int get_value_at_indices_double(void *,const  char*,  double*, int*, int );

extern int set_value_int(void *,const  char*,  int* );
extern int set_value_int1(void *,const  char*,  int8_t* );
extern int set_value_int2(void *,const  char*,  short* );
extern int set_value_int8(void *,const  char*,  long* );
extern int set_value_float(void *,const  char*,  float* );
extern int set_value_double(void *,const  char*,  double* );
extern int set_value_string(void *,const  char*,  char* );
extern int set_value_logical(void *,const  char*,  bool* );

extern int set_value_at_indices_int(void *,const  char*,  int*, int*, int );
extern int set_value_at_indices_int1(void *,const  char*,  int*, int8_t*, int );
extern int set_value_at_indices_int2(void *,const  char*,  int*, short*, int );
extern int set_value_at_indices_int8(void *,const  char*,  int*, long*, int );
extern int set_value_at_indices_logical(void *,const  char*,  int*, bool*, int );
extern int set_value_at_indices_string(void *,const  char*,  int*, char*, int );
extern int set_value_at_indices_float(void *,const  char*,  int*, float*, int );
extern int set_value_at_indices_double(void *,const  char*,  int*, double*, int );

extern int get_grid_rank( void *, int* grid, int* rank );
extern int get_grid_type( void *, int* grid, char* type );
extern int get_grid_size( void *, int* grid, int* size );

extern int get_grid_shape( void *, int* grid, int* shape );
extern int get_grid_spacing( void *, int* grid, double* spacing );
extern int get_grid_origin( void *, int* grid, double* size );

extern int get_grid_x( void *, int* grid, double* x );
extern int get_grid_y( void *, int* grid, double* y );
extern int get_grid_z( void *, int* grid, double* z );

extern int get_grid_node_count(void *, int* grid, int* count);
extern int get_grid_edge_count(void *, int* grid, int* count);
extern int get_grid_face_count(void *, int* grid, int* count);
extern int get_grid_edge_nodes(void *, int* grid, int* edge_nodes);
extern int get_grid_face_edges(void *, int* grid, int* face_edges);
extern int get_grid_face_nodes(void *, int* grid, int* face_nodes);
extern int get_grid_nodes_per_face(void *, int* grid, int* nodes_per_face);


#endif // #ifndef ISO_C_BMIF_H
