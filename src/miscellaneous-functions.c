
#include "miscellaneous-functions.h"
#include "dembase.h"

/* utility functions for debugging printing */
void printDblArray(double *a, int len)
{   
    SEXP tmp_R;
    PROTECT(tmp_R = allocVector(REALSXP, len));
    double *tmp = REAL(tmp_R);
    memcpy(tmp, a, len*sizeof(double));
    PrintValue(tmp_R);
    UNPROTECT(1);
}

void printIntArray(int *a, int len)
{   
    SEXP tmp_R;
    PROTECT(tmp_R = allocVector(INTSXP, len));
    int *tmp = INTEGER(tmp_R);
    memcpy(tmp, a, len*sizeof(int));
    PrintValue(tmp_R);
    UNPROTECT(1);
}

/*
 * rpoisDiffConstr <- function(lambda1, lambda2, min, maxAttempt = 1000L, useC = FALSE) {
    ## lambda1
    stopifnot(is.numeric(lambda1))
    stopifnot(length(lambda1) > 0L)
    stopifnot(all(lambda1[!is.na(lambda1)] >= 0))
    lambda1 <- as.double(lambda1)
    ## lambda2
    stopifnot(is.numeric(lambda2))
    stopifnot(all(lambda2[!is.na(lambda2)] >= 0))
    lambda2 <- as.double(lambda2)
    ## min
    stopifnot(is.integer(min))
    ## maxAttempt
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## lambda1 and lambda2
    stopifnot(identical(length(lambda2), length(lambda1)))
    ## lambda1 and min
    stopifnot(identical(length(min), length(lambda1)))
    ## 
    if (useC) {
        .Call(rpoisDiffConstr_R, lambda1, lambda2, min, maxAttempt)
    }
    else {
        n <- length(lambda1)
        y1 <- integer(length = n)
        y2 <- integer(length = n)
        y3 <- integer(length = n)
        for (i in seq_len(n)) {
            l1 <- lambda1[i]
            l2 <- lambda2[i]
            m <- min[i]
            if (is.na(l1) || is.na(l2) || is.na(m)) {
                y1[i] <- as.integer(NA)
                y2[i] <- as.integer(NA)
                y3[i] <- as.integer(NA)
            }
            else {
                found <- FALSE
                attempt <- 0L
                while (!found && (attempt < maxAttempt)) {
                    attempt <- attempt + 1L
                    prop1 <- rpois(n = 1L, lambda = l1)
                    prop1 <- as.integer(prop1)
                    prop2 <- rpois(n = 1L, lambda = l2)
                    prop2 <- as.integer(prop2)
                    found <- (prop1 - prop2) >= m
                }
                if (found) {
                    y1[i] <- prop1
                    y2[i] <- prop2
                    y3[i] <- prop1 - prop2
                }
                else {
                    y1[i] <- as.integer(NA)
                    y2[i] <- as.integer(NA)
                    y3[i] <- m
                }
            }
        }
        list(y1 = y1, y2 = y2, y3 = y3)
    }
}
*/

/* fills in y1, y2, y3
 * assumes length y1 = length y2 = length y3 = nLambda 
 * = length input lambdas and min */
void rpoisDiffConstr(SEXP lambda1_R, SEXP lambda2_R,
                    SEXP min_R, SEXP maxAttempt_R,
                    int* y1, int* y2, int* y3, int nLambda) 
{
    double *lambda1 = REAL(lambda1_R);
    double *lambda2 = REAL(lambda2_R);
    int *min = INTEGER(min_R);
    int maxAttempt = *INTEGER(maxAttempt_R);
    
    for (int i = 0; i < nLambda; ++i) {
        
        double l1 = lambda1[i];
        double l2 = lambda2[i];
        int m = min[i];
        
        if ( ISNA(l1) || ISNA(l2) || (m == NA_INTEGER)) {
            
            y1[i] = NA_INTEGER;
            y2[i] = NA_INTEGER;
            y3[i] = NA_INTEGER;
        }
        else {
            int found = 0;
            int attempt = 0;
            
            int prop1 = 0;
            int prop2 = 0;
            int diff = 0;
            
            while (!found && (attempt < maxAttempt)) {
                ++attempt;
                prop1 = (int)rpois(l1);
                prop2 = (int)rpois(l2);
                diff = prop1 - prop2;
                found = (diff >= m);
                
            }
            if (found) {
                
                y1[i] = prop1;
                y2[i] = prop2;
                y3[i] = diff;
            }
            else {
                
                y1[i] = NA_INTEGER;
                y2[i] = NA_INTEGER;
                y3[i] = m;
            }
        }
    } /* end for i */
}

/* Helper functions used by 'getIAfter', 'getIBefore' and 'getIShared' */


SEXP
posToMar_R(SEXP pos_R, SEXP dim_R)
{
    int pos = *INTEGER(pos_R);
    
    int *dim = INTEGER(dim_R);
    int ndim = length(dim_R);
    
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, ndim));
    int *ans = INTEGER(ans_R);
    
    getPosToMar (ans, pos, dim, ndim);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

#if(1)
void
getPosToMar (int* ans, int pos, int* dim, int ndim)
{
    pos -= 1;
    
    int div_mod_by = 1;
    
    for (int d = 0; d < ndim; ++d) {
        pos /= div_mod_by;
        div_mod_by = dim[d];
        ans[d] = pos % div_mod_by + 1;
    }
}
#endif

/* alternative using multiplier which avoids mod 
 * would be even better if we were passed mults not dim */
#if(0)
void
getPosToMar (int* ans, int pos, int* dim, int ndim)
{
    pos -= 1;
    
    int mults[ndim];
    int m = 1;
    mults[0] = 1;
    
    for (int d = 1; d < ndim; ++d) {
        m *= dim[d-1];
        mults[d] = m;
    }
    
    for (int d = ndim-1; d >= 0; --d) {
        int m = mults[d];
        int c = pos / m; /* integer division */
        pos -= c*m;
        ans[d] = c + 1; /* add 1 to make R-style coord */
    }
}
#endif

SEXP
marToPos_R(SEXP mar_R, SEXP multiplier_R)
{
    int *mar = INTEGER(mar_R);
    
    int *multiplier = INTEGER(multiplier_R);
    int nmult = length(multiplier_R);
    
    int ans = getMarToPos(mar, multiplier, nmult);
    
    return ScalarInteger(ans);
    
}

int
getMarToPos (int* mar, int* mult, int nmult)
{
    int ans = 1;
    
    for (int i = 0; i < nmult; ++i) {
        
        ans += mult[i] * (mar[i] - 1);
    }
    return ans;
}

SEXP
marBeforeToMarAfter_R(SEXP mar_R, SEXP indices_R, 
                        SEXP dims_R, SEXP dimAfter_R)
{
    int *mar = INTEGER(mar_R);
    
    int *dims = INTEGER(dims_R);
    
    int n_after = length(dimAfter_R);
    
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, n_after));
    int *ans = INTEGER(ans_R);
    
    marBeforeToMarAfter (ans, n_after, mar, dims, 
                        indices_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

void
marBeforeToMarAfter (int* ans, int n_after, int* mar, int* dims, 
                        SEXP indices_R)
{
    int n_before = LENGTH(indices_R); /* len of dims and mar */
    
    for (int i_dim_before = 0; i_dim_before < n_before; ++i_dim_before) {
        int i_margin_before = mar[i_dim_before];
        int *index = INTEGER(VECTOR_ELT(indices_R, i_dim_before));
        int i_margin_after = index[i_margin_before-1];
        if (i_margin_after > 0) {
            int i_dim_after = dims[i_dim_before];
            if (i_dim_after > 0) {
                ans[i_dim_after-1] = i_margin_after;
            }
        }
        else {
            memset(ans, 0, n_after * sizeof(int)); /* set all to 0 */
            break;
        }
    }
}

SEXP
marAfterToPosBefore_R(SEXP mar_R, SEXP dims_R, 
                    SEXP multiplierBefore_R, SEXP invIndices_R)
{
    int *dims = INTEGER(dims_R);
    
    int *mar = INTEGER(mar_R);
    
    int *multiplierBefore = INTEGER(multiplierBefore_R);
    
    return marAfterToPosBefore(dims, mar,  
                    multiplierBefore, invIndices_R);
}


SEXP
marAfterToPosBefore(int *dims, int *mar,  
                    int *multiplierBefore, SEXP invIndices_R)
{
    int n_dim_before = LENGTH(invIndices_R); /* ==length mult, dims */
    
    int inv_indices_margin_ind[n_dim_before];
    memset(inv_indices_margin_ind, 0, n_dim_before*sizeof(int));
    
    int n_indices[n_dim_before];
    
    int n_margin_before = 1;
    
    for (int i_dim_before = 0; i_dim_before < n_dim_before;
                                    ++i_dim_before) {
        int i_inv_index = 0;
        int i_dim_after = dims[i_dim_before];
        if (i_dim_after > 0) {
            i_inv_index = mar[i_dim_after - 1] - 1;
            inv_indices_margin_ind[i_dim_before] = i_inv_index;
        }
        
        int len = LENGTH(VECTOR_ELT(VECTOR_ELT(
                        invIndices_R,i_dim_before),i_inv_index));
        
        n_indices[i_dim_before] = len;
        if (len > 1) n_margin_before *= len;
                                         
    }
    
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, n_margin_before));
    int *ans = INTEGER(ans_R);
    
    int margin_before[n_dim_before];
    int ansPosToMar[n_dim_before];
    
    for (int i_ans = 1; i_ans <= n_margin_before;
                                    ++i_ans) {
        getPosToMar(ansPosToMar, i_ans, n_indices, n_dim_before);
        
        for (int i_dim_before = 0; i_dim_before < n_dim_before;
                                    ++i_dim_before) {
            
            int i_inv_index = inv_indices_margin_ind[i_dim_before];
            int *inv_index = INTEGER(VECTOR_ELT(VECTOR_ELT(
                        invIndices_R,i_dim_before),i_inv_index));
            
            int i_index = ansPosToMar[i_dim_before];
            margin_before[i_dim_before] = inv_index[i_index - 1];
            
        }
        
        ans[i_ans-1] = getMarToPos (margin_before, 
                                    multiplierBefore, n_dim_before);
    }
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}


SEXP
getIAfter_R(SEXP i_R, SEXP transform_R)
{
    int i = *INTEGER(i_R);
    int ans = getIAfter(i, transform_R);
    return ScalarInteger(ans);
    
}

int
getIAfter(int i, SEXP transform_R)
{
    SEXP indices_R = GET_SLOT(transform_R, indices_sym);
    int *dims = INTEGER(GET_SLOT(transform_R, dims_sym));
    SEXP dimBefore_R = GET_SLOT(transform_R, dimBefore_sym);
    int *dim_before = INTEGER(dimBefore_R);
    int n_dim_before = LENGTH(dimBefore_R);
    SEXP multiplierAfter_R = GET_SLOT(transform_R, multiplierAfter_sym);
    int n_mult_after = LENGTH(multiplierAfter_R);
    int *multiplier_after = INTEGER(multiplierAfter_R);
    
    int margin_before[n_dim_before];
    getPosToMar (margin_before, i, dim_before, n_dim_before);  
    
    int margin_after[n_mult_after];
    marBeforeToMarAfter (margin_after, n_mult_after, margin_before,
                        dims, indices_R);
    
    int ans = 0;
    if (margin_after[0] > 0) {
        /* either all non-zero or all zero */
        ans = getMarToPos(margin_after, multiplier_after, n_mult_after);
    }
    return ans;
    
}

SEXP
getIBefore_R(SEXP i_R, SEXP transform_R)
{
    int i = *INTEGER(i_R);
    
    return getIBefore(i, transform_R);
    
}

SEXP
getIBefore(int i, SEXP transform_R)
{
    SEXP invIndices_R = GET_SLOT(transform_R, invIndices_sym);
    int *dims = INTEGER(GET_SLOT(transform_R, dims_sym));
    SEXP dimAfter_R = GET_SLOT(transform_R, dimAfter_sym);
    int *dim_after = INTEGER(dimAfter_R);
    int n_dim_after = LENGTH(dimAfter_R);
    int *multiplier_before = INTEGER(GET_SLOT(transform_R, 
                                            multiplierBefore_sym));
    
    int margin_after[n_dim_after];
    getPosToMar (margin_after, i, dim_after, n_dim_after);  
    
    return marAfterToPosBefore(dims, margin_after,  
                    multiplier_before, invIndices_R);
    
}

SEXP
getIShared_R(SEXP i_R, SEXP transform_R)
{
    int i = *INTEGER(i_R);
    
    return getIShared(i, transform_R);
    
}

SEXP
getIShared(int i, SEXP transform_R)
{
    SEXP indices_R = GET_SLOT(transform_R, indices_sym);
    SEXP invIndices_R = GET_SLOT(transform_R, invIndices_sym);
    
    int *dims = INTEGER(GET_SLOT(transform_R, dims_sym));
    
    SEXP dimBefore_R = GET_SLOT(transform_R, dimBefore_sym);
    int *dim_before = INTEGER(dimBefore_R);
    int n_dim_before = LENGTH(dimBefore_R);
    
    int n_dim_after = LENGTH(GET_SLOT(transform_R, dimAfter_sym));
    
    int *multiplier_before = INTEGER(GET_SLOT(transform_R, 
                                            multiplierBefore_sym));

    int margin_before[n_dim_before];
    getPosToMar (margin_before, i, dim_before, n_dim_before);  
    
    int margin_after[n_dim_after]; 
    marBeforeToMarAfter (margin_after, n_dim_after, margin_before,
                        dims, indices_R);
    
    SEXP ans_R;
    
    if (margin_after[0] > 0) {
        /* either all non-zero or all zero */
        PROTECT(ans_R = marAfterToPosBefore(dims, margin_after, 
                        multiplier_before, invIndices_R));

    }
    else {
        PROTECT(ans_R = allocVector(INTSXP, 0));
    }
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

void
redistributeInnerMeans(SEXP ans_R, SEXP counts_R, SEXP weights_R, SEXP transform_R)
{
    int n = length(weights_R);
    double *ans = REAL(ans_R);
    
    redistributeInnerMeans_Internal(ans, n, counts_R, weights_R, transform_R);
    
    #ifdef DEBUGGING
        PrintValue(mkString("final ans_R"));
        PrintValue(ans_R);
    #endif
}

void
redistributeInnerMeans_Internal(double *ans, int n,
                    SEXP counts_R, SEXP weights_R, SEXP transform_R)
{
    memset(ans, 0, n * sizeof(int)); /* set all to 0.0 */
    
    double *weights = REAL(weights_R);
    int *counts = INTEGER(counts_R);
    int nCounts = LENGTH(counts_R);
    
    double *prob = (double *)R_alloc(n, sizeof(double));
    
    for (int iCount = 0; iCount < nCounts; ++iCount) {
        int sizeCount = counts[iCount];
        
        if (sizeCount > 0) {
            
            int iCountR = iCount+1; /* R style index */
        
            SEXP iWeight_R = getIBefore(iCountR, transform_R);
            
            int nWeight = LENGTH(iWeight_R);
            int *iWeight = INTEGER(iWeight_R);
            
            double sumProb = 0.0;
            
            for (int w = 0; w < nWeight; ++w) {
                double thisProb = weights[ iWeight[w] - 1 ];
                prob[w] = thisProb;
                sumProb += thisProb;
            }
            
            if(sumProb > 0) {
                
                /*  ans is from prob / sumProb * sizeCount */
                double mult = sizeCount / sumProb;  
                for (int w = 0; w < nWeight; ++w) {
                    ans[ iWeight[w] - 1 ] = prob[w] * mult;
                }
                
            }
            else {
                error("weights for element %d of 'counts' sum to 0",
                                 iCountR);
            }
        }
    }
}

void
redistributeInnerDistn(SEXP ans_R, SEXP counts_R, SEXP weights_R, SEXP transform_R)
{
    int n = length(weights_R);
    int *ans = INTEGER(ans_R);
    
    redistributeInnerDistn_Internal(ans, n, counts_R, weights_R, transform_R);
    
    #ifdef DEBUGGING
        PrintValue(mkString("final ans_R"));
        PrintValue(ans_R);
    #endif
}

void
redistributeInnerDistn_Internal(int *ans, int n,
                    SEXP counts_R, SEXP weights_R, SEXP transform_R)
{
    memset(ans, 0, n * sizeof(int)); /* set all to 0.0 */
    
    double *weights = REAL(weights_R);
    int *counts = INTEGER(counts_R);
    int nCounts = LENGTH(counts_R);
    
    double *prob = (double *)R_alloc(n, sizeof(double));
    /* allocate as much space as we might possibly need for newValues */
    int *newValues = (int *)R_alloc(n, sizeof(int));
    
    for (int iCount = 0; iCount < nCounts; ++iCount) {
        int sizeCount = counts[iCount];
        
        if (sizeCount > 0) {
            
            int iCountR = iCount+1; /* R style index */
        
            SEXP iWeight_R = getIBefore(iCountR, transform_R);
            
            int nWeight = LENGTH(iWeight_R);
            int *iWeight = INTEGER(iWeight_R);
            
            double sumProb = 0.0;
            
            for (int w = 0; w < nWeight; ++w) {
                double thisProb = weights[ iWeight[w] - 1 ];
                prob[w] = thisProb;
                sumProb += thisProb;
            }
            
            if(sumProb > 0) {
                
                /* c version of rmultinom gives rubbish if weights not 
                 * normalised */
                for (int w = 0; w < nWeight; ++w) {
                    prob[w] /= sumProb;
                }
                
                rmultinom(sizeCount, prob, nWeight, newValues);
                /* after call, newValues contains vector from rmultinom */
                
                for (int w = 0; w < nWeight; ++w) {
                    ans[ iWeight[w] - 1 ] = newValues[w];
                }
            }
            else {
                error("weights for element %d of 'counts' sum to 0",
                                 iCountR);
            }
        }
    }
}

/* developing and testing (via console printout) of mapping stuff in C */
void
doStuff(SEXP mapObject_R, SEXP componentCoords_R)
{   
    #if(0)
    SEXP indices_R = GET_SLOT(mapObject_R, indices_sym); /* list */
    SEXP dimBefore_R = GET_SLOT(mapObject_R, dimBefore_sym); 
    SEXP dimAfter_R = GET_SLOT(mapObject_R, dimAfter_sym); 
    int *dimBefore = INTEGER(dimBefore_R); 
    int *dimAfter = INTEGER(dimAfter_R); 
    int *map = INTEGER(GET_SLOT(mapObject_R, map_sym)); 
    int *multiplierAfter = INTEGER(GET_SLOT(mapObject_R, multiplierAfter_sym)); 
    
    int nDimBefore = LENGTH(dimBefore_R); 
    int nDimAfter = LENGTH(dimAfter_R); 
    
    /* iXXX are all 0 or R-syle index numbers of dimensions in component */
    int iTime = *INTEGER(GET_SLOT(mapObject_R, iTime_sym)); 
    int iOrigin = *INTEGER(GET_SLOT(mapObject_R, iOrigin_sym)); 
    int iDest = *INTEGER(GET_SLOT(mapObject_R, iDest_sym)); 
    int iDirection = *INTEGER(GET_SLOT(mapObject_R, iDirection_sym)); 
    int iAge = *INTEGER(GET_SLOT(mapObject_R, iAge_sym)); 
    int iTriangle = *INTEGER(GET_SLOT(mapObject_R, iTriangle_sym)); 
    int dUpper = *INTEGER(GET_SLOT(mapObject_R, dUpper_sym)); 
    
    int nCoord = LENGTH(componentCoords_R);
    
    /* aim is to get position(s) of first cells in population affected */
    
    /* array to hold position(s) (1 or 2) in population that we find 
     * I have assumed here that we can get if from number of coordinates
     * sent in, but I am confused whether we really get two coordinates
     * or one for origin-destination components - see notes below 
     * - so may need to change this to checking iOrigin, etc*/
    int positions[nCoord]; 
    
    for (int iCoord = 0; iCoord < nCoord; ++iCoord) {
        
        /* the component coordinate to find the population pos for 
         * again - assumes origin-destination component means two coordinates
         * supplied but I am not sure about this.*/
        int *cCoord_r = INTEGER(VECTOR_ELT(componentCoords_R, iCoord));
            
        /* for each dimension in component */
        for (int iC = 0; iC < nDimBefore; ++iC) {
            
            int iP = map[iC] - 1; /* index of dimension in population */
            /* if iP < 0, there is no corresponding dimension in pop,
             * so dimension presumably corresponds to triangle, direction.
             * etc, but we may still need to deal with it */
            
            if (iP >= 0) {
                
                /* get a dim value on dimension iP in pop */ 
                int value = 0;
                           
                int *index = INTEGER(VECTOR_ELT(indices_R, iC));
                int cCoordValue = cCoord_r[iC] - 1;
            
                int iC_r = iC + 1;
                
/* may have to deal with births explicitly, unless the age 
 * issue is dealt with by indices */
                
                /* if deal with births above, this becomes and else if */
                if (iC_r == iAge) {
                    
                    /*either have already checked if this is a birth event or
                     * indices will take care of everything going to age 0 */
                    
                    int maxAgeValuePop = dimAfter[iP] - 1;
                    
                    value = index[cCoordValue] - 1;
                    
                    
/* how do values on age dim in component, and component triangle dim, and population work together?
 * eg do we have the same intervals for age in component and popn?
 * if not, does index take care of mapping
 * and, assuming that the age intervals are the same, 
 * if say component age interval is equivalent to population age interval (5-10] 
 * if lower triangle
 * but the event coord is in upper triangle 
 * would it map to the next interval 'up' in the population (maybe (10-15]) ? 
 * and if not, how on earth would we know what to make of upper triangle or lower 
 * and it still seems to be messy to me unless the age interval values in component and pop are
 * the same 
 * 
 * I can see the isRegular attribute which applies to age & time in components and populations 
 * but is it the same regularity that applies in both component and pop? */
 
                    /* if component has age it must have triangle dimension */
                    if (cCoord_r[iTriangle] == dUpper && (value < maxAgeValuePop) ) {
                        value += 1;
                    }
                    
                    
                }
                else if (iC_r == iTime) {
                    /* I don't think we need anything special here 
                     * eg value representing first time interval in component 
                     * would go to value representing time (starting at) first
                     * time point in population */  
                    
                }

/* algorithm says that for origin/destination objects, two cells
 * will have been chosen to update 
 * but if origin is a dimension and destination is a dimension, choosing one cell 
 * in the object seems to imply both origin and destination identified?
 * 
 * contrast pool objects where I can see that we do need to choose two cells
 * because direction dimension contains in and out as values so need to choose 
 * different cells - one with direction value == in and one == out*/
                
                /*this approach itself will still work even if one coord sent
                 * in for origin-destination objects - just need to change
                 * how nCoord is set and how iCoord is used above. */
                else if ( (iC_r == iDest) || (iC_r == iOrigin) ) {
                        
                }
                else if ( (iCoord == 1) && (iC_r == iDest) ) {
                        
                }
                    
                /* TODO deal with pool stuff */
                
                else {
                    
                    value = cCoordValue;
                    /* if using indices 
                    value = index[cCoordValue] - 1; 
                    */
                }
            
                positions[iCoord] += value * multiplierAfter[iP];
                
                
            } /* end iP >= 0 */
        } /* end loop through dimensions in a component coord */
            
    } /* end loop through component coords for which positions are to be found */
#endif
}

/* testing - check that a 'collapse' works */

/* get (fill in) the C-style coord corresponding to a position 
 * in an object (eg, population)
 * 
 * the coord array is assumed to be the right length = nDim
 * pos is a C-style postion, ie in range 0 to nElements-1
 * */
void
getCoordFromPos(int* coord, int pos, int* multiplier, int nDim)
{
    for (int i = nDim-1; i >= 0; --i) {
        int m = multiplier[i];
        int c = pos / m; /* integer division */
        pos -= c*m;
        coord[i] = c;
    }
}
