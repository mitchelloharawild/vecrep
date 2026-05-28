#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>
#include <stdio.h>


/*
 ALTREP objects which represent a vector formed by repeating the
 elements of a parent numeric vector a fixed number of times,
 equivalent to base::rep(parent, times = n), without allocating
 the expanded data until absolutely necessary.
 */


static R_altrep_class_t rep_real_class;

/* reps are ALTREPs with data fields

   data1: VECSXP (list) length 2
       0: ExternalPtr canary (parent in Protected slot)
       1: REALSXP (times)
   data2: Expanded data SEXP (initialized to R_NilValue)

   If data2 (expanded SEXP) is ever not R_NilValue (R's NULL), all methods
   must hit that, as it means a writeable dataptr has been given out.

   The canary lets us ensure the reference to parent gets decremented on
   destruction of the altrep IF THIS STILL NEEDS TO OCCUR.

   If a writable dataptr is retrieved, we populate data2 (expanded SEXP),
   then we set the reference to parent in canary ('protected' field) to
   R_NilValue, THEN clear the canary external pointer.

   If the canary is still uncleared upon finalization, we do the cleanup
   then which takes care of recovering the reference count automatically.
 */


#define VREP_PARENT(x)          R_ExternalPtrProtected(VECTOR_ELT(R_altrep_data1(x), 0))
/* We always want to do this as a unit! */
#define FULL_CLEAR_EXTPTR(x) do {                       \
        R_SetExternalPtrProtected(x, R_NilValue);       \
        R_ClearExternalPtr(x);                          \
    } while(0)

/* this decrements the reference count for parent and then */
/* clears the canary */
#define VREP_UNSET_PARENT(x)    FULL_CLEAR_EXTPTR(VECTOR_ELT(R_altrep_data1(x), 0))
#define VREP_PATTERN_LEN(x)     XLENGTH(VREP_PARENT(x))
#define VREP_TIMES(x)           ((R_xlen_t) INTEGER_ELT(VECTOR_ELT(R_altrep_data1(x), 1), 0))
#define VREP_EXPANDED(x)        R_altrep_data2(x)
#define VREP_SET_EXPANDED(x, v) R_set_altrep_data2(x, v)

void canary_finalizer(SEXP x) {
    int *canary = (int *) R_ExternalPtrAddr(x);
    /* check if our canary is still tweeting and hopping about */
    if(canary) {
        FULL_CLEAR_EXTPTR(x);
    }
}

SEXP make_rep_real(SEXP parent, SEXP times) {
    /* carry around a pointer to parent that we can put a finalizer on
       so we're not accumulating reference count that can't be
       decremented.
    */

#ifndef SWITCH_TO_REFCNT
    /* NAMED wouldn't be decremented so in NAMED world just mark parent
       not mutable for safety */
    MARK_NOT_MUTABLE(parent);
#endif
    int *canarydata = malloc(sizeof(int));
    /* there was a bug in R_MakeExternalPtr which didn't increment ref counts of Prot,
       but setter did. Fixed already in R-devel and R-patched but initializing to
       R_NilValue then setting it works backwards-compatibly. */
    SEXP canary = R_MakeExternalPtr(canarydata, R_NilValue, R_NilValue);
    R_SetExternalPtrProtected(canary, parent);
    R_RegisterCFinalizerEx(canary, canary_finalizer, TRUE);
    SEXP mdata = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(mdata, 0, canary);
    SET_VECTOR_ELT(mdata, 1, times);
    R_altrep_class_t cls = rep_real_class;
    SEXP ans = R_new_altrep(cls, mdata, R_NilValue);
    UNPROTECT(1); /* mdata */
    return ans;
}


static SEXP vrep_Serialized_state(SEXP x) {
    /*
     * no serializing rep vectors as altreps,
     * will be converted to std vec
     */
    return NULL;
}



Rboolean vrep_Inspect(SEXP x, int pre, int deep, int pvec,
                      void (*inspect_subtree)(SEXP, int, int, int))
{
    Rprintf(" rep double");
    if(VREP_EXPANDED(x) != R_NilValue)
        Rprintf(" [ expanded ]\n");
    else
        Rprintf(" [par %p pattern_len: %ld times: %ld]\n",
                (void *) VREP_PARENT(x),
                (long) VREP_PATTERN_LEN(x),
                (long) VREP_TIMES(x));
    return TRUE;
}



static R_xlen_t vrep_Length(SEXP x)
{
    SEXP exp = VREP_EXPANDED(x);
    if(exp != R_NilValue) return XLENGTH(exp);
    return VREP_PATTERN_LEN(x) * VREP_TIMES(x);
}

static double vrep_real_Elt(SEXP x, R_xlen_t i) {
    SEXP exp = VREP_EXPANDED(x);
    if(exp != R_NilValue) return REAL_ELT(exp, i);
    return REAL_ELT(VREP_PARENT(x), i % VREP_PATTERN_LEN(x));
}

static
R_xlen_t vrep_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf) {
    SEXP exp = VREP_EXPANDED(x);
    if(exp != R_NilValue) return REAL_GET_REGION(exp, i, n, buf);
    R_xlen_t xlen = XLENGTH(x);
    R_xlen_t ncopy = (xlen - i > n) ? n : xlen - i;
    R_xlen_t pattern_len = VREP_PATTERN_LEN(x);
    const double *parent = REAL_RO(VREP_PARENT(x));
    for(R_xlen_t j = 0; j < ncopy; j++)
        buf[j] = parent[(i + j) % pattern_len];
    return ncopy;
}

static void *vrep_Dataptr(SEXP x, Rboolean writeable)
{
    SEXP exp = VREP_EXPANDED(x);
    if(exp != R_NilValue) {
        /*
         * we already lost our ALTREPness, no sense in pretending
         * otherwise now, just operate on the expanded version
         */
        return REAL(exp);
    }

    R_xlen_t len = vrep_Length(x);
    SEXP ans = PROTECT(allocVector(REALSXP, len));
    double *buf = REAL(ans);

    R_xlen_t pattern_len = VREP_PATTERN_LEN(x);
    const double *parent = REAL_RO(VREP_PARENT(x));
    for(R_xlen_t j = 0; j < len; j++)
        buf[j] = parent[j % pattern_len];

    VREP_SET_EXPANDED(x, ans);

    if(writeable) {
        /*
         * writable path: release the parent canary so we stop holding
         * a reference to the parent vector
         */
        VREP_UNSET_PARENT(x);
    }
    /* read-only path: keep the parent canary alive so the object retains
       access to the parent vector for future ALTREP element access */

    UNPROTECT(1);
    return REAL(ans);
}

static const void *vrep_Dataptr_or_null(SEXP x)
{
    /* already expanded, so just do that */
    SEXP exp = VREP_EXPANDED(x);
    if(exp != R_NilValue) {
        return REAL_RO(exp);
    }

    /* no thanks I like being an ALTREP */
    return NULL;
}

static int vrep_real_Is_sorted(SEXP x) {
    /* a replicated vector with times > 1 is only sorted if pattern_len == 1;
       return UNKNOWN_SORTEDNESS as the safe conservative answer always */
    return UNKNOWN_SORTEDNESS;
}

static int vrep_real_No_NA(SEXP x) {
    if(VREP_EXPANDED(x) != R_NilValue) {
        return 0; /* conservative once expanded */
    }
    return REAL_NO_NA(VREP_PARENT(x));
}

static void InitVRepRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
        R_make_altreal_class("vrep_real", "vecrep", dll);

    rep_real_class = cls;

    /* note the differences after R_set_ below */

    /* ALTREP methods */
    R_set_altrep_Inspect_method(cls, vrep_Inspect);
    R_set_altrep_Length_method(cls, vrep_Length);
    R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

    /* ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, vrep_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, vrep_Dataptr_or_null);

    /* ALTREAL methods */
    R_set_altreal_Elt_method(cls, vrep_real_Elt);
    R_set_altreal_Get_region_method(cls, vrep_real_Get_region);
    R_set_altreal_Is_sorted_method(cls, vrep_real_Is_sorted);
    R_set_altreal_No_NA_method(cls, vrep_real_No_NA);
}

/*
 * Shared Library Initialization and Finalization
 */

static const R_CallMethodDef CallEntries[] = {
    {"make_rep_real", (DL_FUNC) &make_rep_real, -1},
    {NULL, NULL, 0}
};

void R_init_vecrep(DllInfo *dll)
{
    InitVRepRealClass(dll);

    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
