
#include "dembase.h"
#include <R_ext/Rdynload.h>



/* one off wrapper for rpoisDiffConstr_R */
SEXP rpoisDiffConstr_R(SEXP lambda1_R, SEXP lambda2_R, 
                        SEXP min_R, SEXP maxAttempt_R)    
{
    
    int nLambda = LENGTH(lambda1_R);
    
    SEXP ans_R;
    int nList = 3;
    /* set up a list with space for 3 elements */
    PROTECT(ans_R = allocVector(VECSXP, nList));
        
    SET_VECTOR_ELT(ans_R, 0, allocVector(INTSXP, nLambda));
    SET_VECTOR_ELT(ans_R, 1, allocVector(INTSXP, nLambda));
    SET_VECTOR_ELT(ans_R, 2, allocVector(INTSXP, nLambda));
    
    int* y1 = INTEGER(VECTOR_ELT(ans_R, 0));
    int* y2 = INTEGER(VECTOR_ELT(ans_R, 1));
    int* y3 = INTEGER(VECTOR_ELT(ans_R, 2));
    
    GetRNGstate();
    /* nothing other than y1, y2, y3 should change */    
    rpoisDiffConstr(lambda1_R, lambda2_R,
                    min_R, maxAttempt_R,
                    y1, y2, y3, nLambda);
    PutRNGstate();
    
    /* set returned list's names for R. */
    char *names[] = {"y1", "y2", "y3"};
    SEXP ret_names_R;
    PROTECT(ret_names_R = allocVector(STRSXP, nList));
    
    for(int i = 0; i < nList; i++){
        SET_STRING_ELT(ret_names_R, i, mkChar(names[i])); 
    }
    SET_NAMES(ans_R, ret_names_R);
    
    UNPROTECT(2); /* ans_R, the names*/
    
    return ans_R;
}

/* wrap redistribute inner */
SEXP redistributeInnerDistn_R(SEXP counts_R, SEXP weights_R, SEXP transform_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, length(weights_R)));
    GetRNGstate();
    redistributeInnerDistn(ans_R, counts_R, weights_R, transform_R);
    PutRNGstate();
    UNPROTECT(1);
    return ans_R;
}

/* wrap redistribute inner */
SEXP redistributeInnerMeans_R(SEXP counts_R, SEXP weights_R, SEXP transform_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, length(weights_R)));
    redistributeInnerMeans(ans_R, counts_R, weights_R, transform_R);
    UNPROTECT(1);
    return ans_R;
}

/* wrap mapping code access point */
SEXP doStuff_R(SEXP mapObject_R, SEXP componentCoord_R)
{
    doStuff(mapObject_R, componentCoord_R);
    return ScalarInteger(0);
}

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const
R_CallMethodDef callMethods[] = {
  CALLDEF(collapse_R, 2),
  CALLDEF(extend_R, 2),
  CALLDEF(get_transformed_index_R, 2),
  CALLDEF(get_affecting_indices_R, 2),
  CALLDEF(get_related_indices_R, 2),
  
  CALLDEF(rpoisDiffConstr_R,4),
  
  CALLDEF(posToMar_R, 2),
  CALLDEF(marToPos_R, 2),
  CALLDEF(marBeforeToMarAfter_R, 4),
  CALLDEF(marAfterToPosBefore_R, 4),
  CALLDEF(getIAfter_R, 2),
  CALLDEF(getIBefore_R,2),
  CALLDEF(getIShared_R,2),
  
  CALLDEF(redistributeInnerMeans_R, 3),
  CALLDEF(redistributeInnerDistn_R, 3),
  
  /* mapping stuff */
  CALLDEF(doStuff_R, 2),
  
  {NULL}
};

#define ADD_SYM(name) name##_sym = install(#name)


/* Macro to make routines are callable from other packages' C code: */

#define RREGDEF(name)  R_RegisterCCallable("dembase", #name, (DL_FUNC) name)

void
R_init_dembase(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);

  /* register functions as callable from other packages 
   * make sure each of these is in the header file in inst/include. */
  RREGDEF(collapse_R);
  RREGDEF(extend_R);
  RREGDEF(getIAfter);
  RREGDEF(getIBefore);
  RREGDEF(getIShared);
  
  /* install symbols */ 
  ADD_SYM(indices);
  ADD_SYM(dims);
  ADD_SYM(dimBefore);
  ADD_SYM(dimAfter);
  
  ADD_SYM(invIndices);
  ADD_SYM(multiplierBefore);
  ADD_SYM(multiplierAfter);
  
  ADD_SYM(map);
  ADD_SYM(iTime);
  ADD_SYM(iOrigin);
  ADD_SYM(iDest);
  ADD_SYM(iDirection);
  ADD_SYM(iAge);
  ADD_SYM(iTriangle);
  ADD_SYM(dUpper);

}
