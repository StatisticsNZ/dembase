
#include "dembase.h"

SEXP collapse_R(SEXP A, SEXP transform) {
    SEXP indices;

    int typeofArray;
    int ndims; /* number of dimensions of A */
    int *dims; /* vector with dimension in A */
    int *stride; /* number of elements from one plane to the next in A */

    SEXP res; /* resulting array */
    int res_ndims; /* number of dimensions of res */
    SEXP res_dims; /* SEXP vector with dimensions of res */
    int *res_stride; /* number of elements from one plane to the next in res */
    
    int *dim_trans; /* permutation of dimensions from A to res */
    int **trans_ptr; /* `ndims' vectors of length `dims[i]' describing
                        transformation from A to res (prior to dimension
                        permutation). */

    double *A_ptrd = NULL;
    double *res_ptrd = NULL;
    int *A_ptri = NULL;
    int *res_ptri = NULL;

    indices = GET_SLOT(transform, indices_sym);
    res_dims = GET_SLOT(transform, dimAfter_sym);
    dim_trans = INTEGER(GET_SLOT(transform, dims_sym));
    res_ndims = LENGTH(res_dims);

    ndims = length(GET_DIM(A));
    dims = INTEGER(GET_DIM(A));
    typeofArray = TYPEOF(A);

    for (int i=0; i<res_ndims; i++) {
        if (INTEGER(res_dims)[i] == 0)
            return allocArray(typeofArray, res_dims);
    }

    /* getting transformation data into trans_ptr array and computing
     * resulting array dimensions as res_dims */
    trans_ptr = (int **)R_alloc(ndims, sizeof(int*));
    for (int i=0; i<ndims; i++)
        trans_ptr[i] = INTEGER(VECTOR_ELT(indices, i));

    /* creating & initializing result array */
    PROTECT(res = allocArray(typeofArray, res_dims));
    switch(typeofArray) {
    case REALSXP:
        A_ptrd = REAL(A);
        res_ptrd = REAL(res);
        memset(res_ptrd, 0, sizeof(double)*length(res));
        break;
    case INTSXP:
        A_ptri = INTEGER(A);
        res_ptri = INTEGER(res);
        memset(res_ptri, 0, sizeof(int)*length(res));
        break;
    default:
        error("can only handle real and integer array types");
    }

    stride = (int *)R_alloc(ndims, sizeof(int));
    stride[0] = 1;
    for (int i=1; i<ndims; i++)
        stride[i] = stride[i-1] * dims[i-1];

    res_stride = (int *)R_alloc(res_ndims, sizeof(int));
    res_stride[0] = 1;
    for (int i=1; i<res_ndims; i++)
        res_stride[i] = res_stride[i-1] * INTEGER(res_dims)[i-1];

    /* setting up for iteration through all elements of A */
    int *pos = (int *)R_alloc(ndims, sizeof(int));
    memset(pos, 0, sizeof(int)*ndims);

    int last_modified = ndims-1;
    int skipping, done = 0;
    while (!done) {

        skipping = 0;
        for (int i=last_modified; i>=0; i--) {
            if (trans_ptr[i][pos[i]] == 0) {
                A_ptri += stride[i];
                A_ptrd += stride[i];
                skipping = 1;
                last_modified = i;
                break;
            }
        }

        if (!skipping) {
            /* computing index into result array */
            double *out_ptrd = res_ptrd;
            int *out_ptri = res_ptri;
            for (int i=0; i<ndims; i++) {
                if (dim_trans[i] == -1) continue;
                int axis_loc = trans_ptr[i][pos[i]] - 1;
                out_ptri += axis_loc * res_stride[dim_trans[i] - 1];
                out_ptrd += axis_loc * res_stride[dim_trans[i] - 1];
            }
            /* actual addition occurs here */
            switch (typeofArray) {
            case REALSXP: *out_ptrd += *A_ptrd++; break;
            case INTSXP:
                if (*out_ptri == NA_INTEGER || *A_ptri == NA_INTEGER)
                    *out_ptri = NA_INTEGER, A_ptri++;
                else
                    *out_ptri += *A_ptri++;
                break;
            }

            last_modified = 0;
        }

        /* updating pos */
        pos[last_modified] += 1;
        for (int i=last_modified; i<ndims; i++) {
            if (pos[i] == dims[i]) {
                if (i == ndims-1) {
                    done = 1;
                    break;
                }
                pos[i] = 0;
                pos[i+1] += 1;
                last_modified = i+1;
            } else {
                break;
            }
        }
    }

    UNPROTECT(1); /* res */

    return res;
}

/* A is an integer or double array to be extended.

   transform is an S4 class with the following slots:

     @dimBefore: The dimensions of A (n dimensions, say)

     @dimAfter: The dimensions of the result array (m dimensions, say)

     @dims: An int vector the same size as @dimAfter that contains the
       mapping of dimensions of A to the result. All values must be in the
       range 1 <= x <= n.

     @indices: A vector containing m int vectors. The ith int vector must
       have @dimsAfter[i] values, where each value is in the range
       1 <= x <= @dimsBefore[@dims[i]]

*/
SEXP
extend_R(SEXP A, SEXP transform)
{

    SEXP indices;

    int typeofArray;
    int ndims; /* number of dimensions of A */
    int *dims; /* vector with dimension in A */
    int *stride; /* number of elements from one plane to the next in A */

    SEXP res; /* resulting array */
    int res_ndims; /* number of dimensions of res */
    SEXP res_dims; /* SEXP vector with dimensions of res */
    
    int *dim_trans; /* permutation of dimensions from A to res */
    int **trans_ptr; /* `ndims' vectors of length `dims[i]' describing
                        transformation from A to res (prior to dimension
                        permutation). */

    double *A_ptrd = NULL;
    double *res_ptrd = NULL;
    double *in_ptrd = NULL;
    int *A_ptri = NULL;
    int *res_ptri = NULL;
    int *in_ptri = NULL;

    indices = GET_SLOT(transform, indices_sym);
    res_dims = GET_SLOT(transform, dimAfter_sym);
    dim_trans = INTEGER(GET_SLOT(transform, dims_sym));
    res_ndims = LENGTH(res_dims);

    ndims = length(GET_DIM(A));
    dims = INTEGER(GET_DIM(A));
    typeofArray = TYPEOF(A);

    /* check on dimAfter */
    int *res_dims_C = INTEGER(res_dims);
    for (int i = 0; i < res_ndims; ++i) {
        if (res_dims_C[i] == 0)
            return allocArray(typeofArray, res_dims);
    }

    /* getting transformation data into trans_ptr array and computing
     * resulting array dimensions as res_dims */
    trans_ptr = (int **)R_alloc(res_ndims, sizeof(int*));
    for (int i=0; i<res_ndims; i++) {
        trans_ptr[i] = INTEGER(VECTOR_ELT(indices, i));
    }

    /* creating & initializing result array */
    PROTECT(res = allocArray(typeofArray, res_dims));
    switch(typeofArray) {
    case REALSXP:
        A_ptrd = REAL(A);
        res_ptrd = REAL(res);
        memset(res_ptrd, 0, sizeof(double)*length(res));
        break;
    case INTSXP:
        A_ptri = INTEGER(A);
        res_ptri = INTEGER(res);
        memset(res_ptri, 0, sizeof(int)*length(res));
        break;
    default:
        error("can only handle real and integer array types");
    }

    stride = (int *)R_alloc(ndims, sizeof(int));
    stride[0] = 1;
    for (int i=1; i<ndims; i++) {
        stride[i] = stride[i-1] * dims[i-1];
    }

    int *pos = (int *)R_alloc(res_ndims, sizeof(int));
    memset(pos, 0, res_ndims*sizeof(int));
    
    int done = 0;
    while (!done) {

        /* computing index into A and setting value in res */
        switch (typeofArray) {
            case REALSXP:
                in_ptrd = A_ptrd;
                for (int i=0; i<res_ndims; i++) {
                    if (dim_trans[i] == 0) {
                        continue;
                    }
                    
                    int axis_loc = trans_ptr[i][pos[i]] - 1;
                    in_ptrd += axis_loc * stride[dim_trans[i] - 1];
                }
                *res_ptrd++ = *in_ptrd;
                break;
            case INTSXP:
                in_ptri = A_ptri;
                for (int i=0; i<res_ndims; i++) {
                    if (dim_trans[i] == 0) {
                        continue;
                    }
                    int axis_loc = trans_ptr[i][pos[i]] - 1;
                    in_ptri += axis_loc * stride[dim_trans[i] - 1];
                }
                *res_ptri++ = *in_ptri;
                break;
        }
    
        /* updating pos */
        pos[0] += 1;
        for (int i=0; i<res_ndims; i++) {
            if (pos[i] == INTEGER(res_dims)[i]) {
                if (i == res_ndims-1) {
                    done = 1;
                    break;
                }
                pos[i] = 0;
                pos[i+1] += 1;
            } 
            else {
                break;
            }
        }
    }
    
    UNPROTECT(1); /* res */

    return res;
}

/* This function works out the index into the transformed array given a
 * transform of type CollapseTransform and an index into the original array,
 *
 * Both indices are int vectors into their respective arrays.
 * */

SEXP
get_transformed_index_R(SEXP x_R, SEXP transform)
{
    int *x = INTEGER(x_R);
    SEXP indices = GET_SLOT(transform, indices_sym);
    int A_ndims = LENGTH(GET_SLOT(transform, dimBefore_sym));
    int B_ndims = LENGTH(GET_SLOT(transform, dimAfter_sym));
    int *dim_trans = INTEGER(GET_SLOT(transform, dims_sym));

    SEXP res_R;
    PROTECT(res_R = allocVector(INTSXP, B_ndims));
    int *res = INTEGER(res_R);

    for (int i=0; i<A_ndims; i++) {
        if (dim_trans[i] == 0) continue;
        int *trans = INTEGER(VECTOR_ELT(indices, i));
        if (trans[x[i] - 1] == 0) {
            UNPROTECT(1);
            return R_NilValue;
        }
        res[dim_trans[i] - 1] = trans[x[i] - 1];
    }

    UNPROTECT(1);

    return res_R;
}

/* This function finds the set of indices in the original array that
 * affect an index in the transformed array. This calculation is based
p * on a transform of type CollapseTransform.
 *
 * The provided index is a single int vector.
 *
 * The function returns a list of int vectors whose length is the number of
 * dimensions of the original array. The elements of the ith vector in the
 * list are those hyperplanes in the ith dimension of the original array that
 * affect the provided index.
 *
 * The total number of elements that affect the provided index can be
 * calculated by multiplying the lengths of the vectors in the list together.
 * */

SEXP
get_affecting_indices_R(SEXP y_R, SEXP transform)
{
    int *y = INTEGER(y_R);
    SEXP indices = GET_SLOT(transform, indices_sym);
    int A_ndims = LENGTH(GET_SLOT(transform, dimBefore_sym));
    int *A_dims = INTEGER(GET_SLOT(transform, dimBefore_sym));
    int *dim_trans = INTEGER(GET_SLOT(transform, dims_sym));

    SEXP res;
    PROTECT(res = allocVector(VECSXP, A_ndims));
    for (int i=0; i<A_ndims; i++) {
        int removed_dim = (dim_trans[i] == 0);
        int *trans_i = INTEGER(VECTOR_ELT(indices, i));

        int n_elem = 0;
        for (int j=0; j<A_dims[i]; j++) {
            if (trans_i[j] == (removed_dim ? 1 : y[dim_trans[i] - 1]))
                n_elem++;
        }

        SEXP new_vec_R = allocVector(INTSXP, n_elem);
        SET_VECTOR_ELT(res, i, new_vec_R);
        int *new_vec = INTEGER(new_vec_R);

        int pos = 0;
        for (int j=0; j<A_dims[i]; j++) {
            if (trans_i[j] == (removed_dim ? 1 : y[dim_trans[i] - 1]))
                new_vec[pos++] = j + 1;
        }
    }

    UNPROTECT(1);
    return res;
}

/* This function finds the set of indices in the original array that
 *   a) the provided index is part of, and
 *   b) all affect the same element in the transformed array.
 * This calculation is based on a transform of type CollapseTransform.
 *
 * If the provided index does not affect any elements in the transformed
 * array, then we return a set of size 1 containing just the provided index.
 *
 * The provided index is a single int vector.
 *
 * The function returns a list of int vectors whose length is the number of
 * dimensions of the original array. The elements of the ith vector in the
 * list are those hyperplanes in the ith dimension of the original array that
 * all affect the same element.
 *
 * The total number of elements that affect the provided index can be
 * calculated by multiplying the lengths of the vectors in the list together.
 */

SEXP
get_related_indices_R(SEXP x_R, SEXP transform)
{
    int *x = INTEGER(x_R);
    SEXP indices = GET_SLOT(transform, indices_sym);
    int A_ndims = LENGTH(GET_SLOT(transform, dimBefore_sym));
    int *A_dims = INTEGER(GET_SLOT(transform, dimBefore_sym));

    SEXP res;
    PROTECT(res = allocVector(VECSXP, A_ndims));

    /* check if x does not show up in the transformed array */
    for (int i=0; i<A_ndims; i++) {
        int *trans_i = INTEGER(VECTOR_ELT(indices, i));
        if (trans_i[x[i] - 1] == 0) {
            /* return x as a list (equiv: as.list(x)) */
            for (int j=0; j<A_ndims; j++) {
                SEXP new_vec_R = allocVector(INTSXP, 1);
                SET_VECTOR_ELT(res, j, new_vec_R);
                INTEGER(new_vec_R)[0] = x[j];
            }
            UNPROTECT(1);
            return res;
        }
    }

    for (int i=0; i<A_ndims; i++) {
        int *trans_i = INTEGER(VECTOR_ELT(indices, i));

        int n_elem = 0;
        for (int j=0; j<A_dims[i]; j++) {
            if (trans_i[j] == trans_i[x[i] - 1])
                n_elem++;
        }

        SEXP new_vec_R = allocVector(INTSXP, n_elem);
        SET_VECTOR_ELT(res, i, new_vec_R);
        int *new_vec = INTEGER(new_vec_R);

        int pos = 0;
        for (int j=0; j<A_dims[i]; j++) {
            if (trans_i[j] == trans_i[x[i] - 1])
                new_vec[pos++] = j + 1;
        }
    }

    UNPROTECT(1);
    return res;
}

