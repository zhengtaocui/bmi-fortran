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
int Get_var_names (Bmi *self, const char *role, char **names);
int Get_var_length (Bmi *self, const char *name, int *size);
int Get_var_type (Bmi *self, const char *name, char *type);
int Get_value_ptr (Bmi *self, const char *name, void **ptr);
int Get_value (Bmi *self, const char *name, void *dest);
int Set_value (Bmi *self, const char *name, void *src_ptr );
int Update (Bmi *self);
int Get_component_name (Bmi *self, char * name);
int Get_start_time (Bmi *self, double * time);
int Get_end_time (Bmi *self, double * time);
int Get_current_time (Bmi *self, double * time);
int Finalize (Bmi *self);

Bmi* register_bmi_fortran(Bmi *model, void* box_handle);

#if defined(__cplusplus)
}
#endif

#endif
