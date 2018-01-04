/* Header for routines that can be called by other C packages.
 * Each of these must be registered as callable in the initialisation
 * code */

#ifndef _TRANSFORM_H
#define _TRANSFORM_H

#include <Rdefines.h>

SEXP collapse_R(SEXP A, SEXP transform);
SEXP extend_R(SEXP A, SEXP transform);

#endif

