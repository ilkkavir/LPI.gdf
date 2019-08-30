// file:LPIgdf.h
// (c) 2010- University of Oulu, Finland
// Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
// Licensed under FreeBSD license.

// Data types and function prototypes

#include <R.h>
#include <math.h>
#include <stdint.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

// gdf file input
SEXP read_gdf_data_R( SEXP ndata , SEXP nfiles , SEXP filepaths , SEXP istart , SEXP iend , SEXP bigendian , SEXP controlbits);
SEXP read_gdf_data( SEXP cata , SEXP idatar , SEXP idatai , SEXP ndata , SEXP nfiles, SEXP filepaths , SEXP istart , SEXP iend , SEXP bigendian , SEXP controlbits);

