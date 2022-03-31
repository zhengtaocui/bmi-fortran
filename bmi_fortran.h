/** ----------------------------------------------
  * bmi_fortran.h
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Mar. 4, 2022
  * Last date of modification: Mar 10, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: Function declarations for bmi_fortran.c.
  *
  * Last modified on Mar 18, 2022
  * Description: renamed the register_bmi_fortran function.
  *              
  */
#ifndef BMI_FORTRAN_H
#define BMI_FORTRAN_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "iso_c_bmif_2_0.h"
#include "bmi.h"

int Initialize(Bmi *self, const char* file );
int Get_var_count (Bmi *self, const char *role, int *count);
int Get_var_index (Bmi *self, const char *name, int *index);
int Get_var_names (Bmi *self, const char *role, char **names);
int Get_var_length (Bmi *self, const char *name, int *size);
int Get_var_type (Bmi *self, const char *name, char *type);
int Get_var_role (Bmi *self, const char *name, char *role);
int Get_value_ptr (Bmi *self, const char *name, void **ptr);
int Get_value (Bmi *self, const char *name, void *dest);
int Set_value (Bmi *self, const char *name, void *src_ptr );
int Update (Bmi *self);
int Update_until (Bmi *self, double then);
int Get_component_name (Bmi *self, char * name);
int Get_start_time (Bmi *self, double * time);
int Get_end_time (Bmi *self, double * time);
int Get_current_time (Bmi *self, double * time);
int Finalize (Bmi *self);

int Get_input_item_count (Bmi *self, int *count);
int Get_output_item_count (Bmi *self, int *count);
int Get_input_var_names (Bmi *self, char **names);
int Get_output_var_names (Bmi *self, char **names);
int Get_var_grid(Bmi *self, const char *name, int *grid);
int Get_var_itemsize(Bmi *self, const char *name, int *size);
int Get_var_nbytes(Bmi *self, const char *name, int *nbytes);
int Get_var_location(Bmi *self, const char *name, char *location);
int Get_time_units (Bmi *self, char * units);
int Get_time_step (Bmi *self, double * dt);
int Get_value_at_indices (Bmi *self, const char *name, void *dest, int *inds, int len);
int Set_value_at_indices (Bmi *self, const char *name, int *inds, int len, void *dest);

int Get_grid_type(Bmi *self, int grid, char * type);
int Get_grid_rank(Bmi *self, int grid, int * rank);
int Get_grid_size(Bmi *self, int grid, int * size);

int Get_grid_shape(Bmi *self, int grid, int *shape);
int Get_grid_spacing(Bmi *self, int grid, double *spacing);
int Get_grid_origin(Bmi *self, int grid, double *origin);

int Get_grid_x(Bmi *self, int grid, double *x);
int Get_grid_y(Bmi *self, int grid, double *y);
int Get_grid_z(Bmi *self, int grid, double *z);

int Get_grid_node_count(Bmi *self, int grid, int *count);
int Get_grid_edge_count(Bmi *self, int grid, int *count);
int Get_grid_face_count(Bmi *self, int grid, int *count);
int Get_grid_edge_nodes(Bmi *self, int grid, int *edge_nodes);
int Get_grid_face_edges(Bmi *self, int grid, int *face_edges);
int Get_grid_face_nodes(Bmi *self, int grid, int *face_nodes);
int Get_grid_nodes_per_face(Bmi *self, int grid, int *nodes_per_face);


Bmi* create_bmi_fortran_model_handle(Bmi *model, void* box_handle);

#if defined(__cplusplus)
}
#endif

#endif
