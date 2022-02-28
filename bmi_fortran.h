#ifndef BMI_FORTRAN_H
#define BMI_FORTRAN_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "iso_c_bmif_2_0.h"
#include "bmi.h"

int Initialize(Bmi *self, const char* file );

// Bmi* register_bmi(Bmi *model);

Bmi* register_bmi_fortran(Bmi *model, void* box_handle);


#if defined(__cplusplus)
}
#endif

#endif
