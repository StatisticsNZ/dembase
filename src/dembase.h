
#ifndef _DEMBASE_H_
#define _DEMBASE_H_

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>


//#define DEBUGGING

/* everything in "x_sym" form here must be macro defined in init.c */
SEXP indices_sym,
     dims_sym,
     dimBefore_sym,
     dimAfter_sym,
     invIndices_sym,
     multiplierBefore_sym,
     multiplierAfter_sym,
     map_sym,
     iTime_sym,
     iOrigin_sym,
     iDest_sym,
     iDirection_sym,
     iAge_sym,
     iTriangle_sym,
     dUpper_sym;


/* array transformations */

SEXP collapse_R(SEXP A, SEXP transform);
SEXP extend_R(SEXP A, SEXP transform);
SEXP get_transformed_index_R(SEXP x, SEXP transform);
SEXP get_affecting_indices_R(SEXP y, SEXP transform);
SEXP get_related_indices_R(SEXP x, SEXP transform);


void rpoisDiffConstr(SEXP lambda1_R, SEXP lambda2_R,
                    SEXP min_R, SEXP maxAttempt_R,
                    int* y1, int* y2, int* y3, int nLambda);

SEXP posToMar_R(SEXP pos_R, SEXP dim_R);
SEXP marToPos_R(SEXP mar_R, SEXP multiplier_R);
SEXP marBeforeToMarAfter_R(SEXP mar_R, SEXP indices_R, 
                        SEXP dims_R, SEXP dimAfter_R);
SEXP marAfterToPosBefore_R(SEXP mar_R, SEXP dims_R, 
                    SEXP multiplierBefore_R, SEXP invIndices_R);
SEXP getIAfter_R(SEXP i_R, SEXP transform_R);
SEXP getIBefore_R(SEXP i_R, SEXP transform_R);
SEXP getIShared_R(SEXP i_R, SEXP transform_R);
void redistributeInnerDistn(SEXP ans_R, SEXP counts_R, 
                        SEXP weights_R, SEXP transform_R);
void redistributeInnerMeans(SEXP ans_R, SEXP counts_R, 
                        SEXP weights_R, SEXP transform_R);
                    
/* these following 3 are 'internal' C functions but in header
 * because used by demest*/
int getIAfter(int i, SEXP transform_R);
SEXP getIBefore(int i, SEXP transform_R);
SEXP getIShared(int i, SEXP transform_R);

/* mapping stuff */
void doStuff(SEXP mapObject_R, SEXP componentCoords_R);    

#endif
