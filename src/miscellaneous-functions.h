
#ifndef __MISC_FUNCIONS_H__
#define __MISC_FUNCIONS_H__

    #include <Rinternals.h>
    
    /* utility functions for debugging printing */
    void printDblArray(double *a, int len);
    void printIntArray(int *a, int len);

    
    void getPosToMar (int* ans, int pos, int* dim, int ndim);
    
    int getMarToPos (int* mar, int* mult, int nmult);
    
    void marBeforeToMarAfter (int* ans, int n_after, int* mar, int* dims, 
                                SEXP indices_R);
    
    SEXP marAfterToPosBefore(int *dims, int *mar,  
                    int *multiplierBefore, SEXP invIndices_R);
    
    void redistributeInnerMeans_Internal(double *ans, int n,
                    SEXP counts_R, SEXP weights_R, SEXP transform_R);

    void redistributeInnerDistn_Internal(int *ans, int n,
                    SEXP counts_R, SEXP weights_R, SEXP transform_R);

    void getCoordFromPos(int* coord, int pos, int* multiplier, int nDim);
    
#endif
