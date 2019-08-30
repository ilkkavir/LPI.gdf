// file:register.c
// (c) 2010- University of Oulu, Finland
// Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
// Licensed under FreeBSD license.

// R registration of C functions

#include "LPIgdf.h"
static const R_CallMethodDef callMethods[2] = {
  { "read_gdf_data_R"       , (DL_FUNC) & read_gdf_data_R       , 7 } , 
  { NULL , NULL , 0 }
};

void R_init_LPIgdf(DllInfo *info)
{
  R_registerRoutines( info , NULL , callMethods , NULL , NULL );
}


