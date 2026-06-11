#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>
#include <stdio.h>
#include <string.h>

/*
 * ALTREP objects representing a vector formed by repeating the elements of a
 * parent vector, equivalent to base::rep(parent, times = t, each = e),
 * without allocating the expanded data until absolutely necessary.
 *
 * Supported types: REALSXP, INTSXP, LGLSXP, CPLXSXP, RAWSXP, STRSXP, VECSXP.
 *
 * Classed vectors (factor, Date, POSIXct, IDate, …) are handled transparently:
 * the class (and dim/dimnames if present) attributes are copied from the parent
 * onto the altrep object itself and onto any expanded materialization.
 *
 * ALTLIST support requires R >= 4.2.
 */

/* ── per-type ALTREP class handles ─────────────────────────────────────── */
static R_altrep_class_t rep_real_class;
static R_altrep_class_t rep_int_class;
static R_altrep_class_t rep_lgl_class;
static R_altrep_class_t rep_cplx_class;
static R_altrep_class_t rep_raw_class;
static R_altrep_class_t rep_str_class;
static R_altrep_class_t rep_list_class;

/* ── data layout ────────────────────────────────────────────────────────────
 *
 *  data1: VECSXP (list) length 3
 *      [0]: ExternalPtr canary  (parent held in Protected slot)
 *      [1]: INTSXP scalar       (times  – whole-pattern repetitions)
 *      [2]: INTSXP scalar       (each   – per-element repetitions)
 *  data2: Expanded data SEXP    (R_NilValue until materialised)
 *
 *  The index mapping for element i of the result is:
 *
 *      parent[ (i / each) % plen ]
 *
 *  Total length = plen * each * times.
 *
 *  Once data2 != R_NilValue every method must use it: a writable Dataptr has
 *  been vended and the buffer may have been mutated.
 *
 *  The canary ensures the parent's reference count is decremented even if the
 *  altrep outlives its creator.  On a writable-Dataptr call we release parent
 *  immediately; on GC finalization the finalizer releases it if still held.
 * ──────────────────────────────────────────────────────────────────────────*/

#define VREP_PARENT(x)      R_ExternalPtrProtected(VECTOR_ELT(R_altrep_data1(x), 0))
#define VREP_CANARY(x)      VECTOR_ELT(R_altrep_data1(x), 0)
#define VREP_TIMES(x)       ((R_xlen_t)INTEGER_ELT(VECTOR_ELT(R_altrep_data1(x), 1), 0))
#define VREP_EACH(x)        ((R_xlen_t)INTEGER_ELT(VECTOR_ELT(R_altrep_data1(x), 2), 0))
#define VREP_PATTERN_LEN(x) XLENGTH(VREP_PARENT(x))
#define VREP_EXPANDED(x)    R_altrep_data2(x)
#define VREP_SET_EXPANDED(x, v) R_set_altrep_data2(x, v)

/* Release the ExternalPtr and its protected parent as one atomic step. */
#define FULL_CLEAR_EXTPTR(ptr)                    \
  do {                                            \
    R_SetExternalPtrProtected((ptr), R_NilValue); \
    R_ClearExternalPtr(ptr);                      \
  } while (0)

#define VREP_UNSET_PARENT(x) FULL_CLEAR_EXTPTR(VREP_CANARY(x))

/* ── canary finalizer ───────────────────────────────────────────────────── */

/* Static address used solely as a non-NULL sentinel; never dereferenced. */
static int canary_sentinel;

static void canary_finalizer(SEXP x) {
  if (R_ExternalPtrAddr(x))   /* sentinel address means "not yet cleared" */
    FULL_CLEAR_EXTPTR(x);
}

/* ── constructor (shared) ───────────────────────────────────────────────── */

/*
 * Copy selected attributes from src to dst.
 * We copy the full set of common vector attributes so that classed vectors
 * (factor, Date, POSIXct, IDate, ts, …) behave correctly when the altrep
 * is inspected by R-level code that dispatches on class().
 *
 *   class    – S3 dispatch
 *   levels   – factor, ordered
 *   dim      – matrices / arrays
 *   dimnames – matrices / arrays
 *   tsp      – ts objects (start, end, frequency)
 *   comment  – comment() metadata
 *
 * "names" is intentionally excluded: its correct value for the repeated vector
 * has length pattern_len * each * times, so it cannot be copied as-is from the
 * parent.  R will compute names lazily on materialisation via the normal
 * attribute lookup path.
 */
static void copy_vector_attrs(SEXP dst, SEXP src) {
  static const char *attrs[] = {
    "class", "levels", "dim", "dimnames", "tsp", "comment", NULL
  };
  for (int i = 0; attrs[i]; i++) {
    SEXP a = getAttrib(src, install(attrs[i]));
    if (a != R_NilValue)
      setAttrib(dst, install(attrs[i]), a);
  }
}

static SEXP make_vrep_internal(SEXP parent, SEXP times, SEXP each,
                                R_altrep_class_t cls) {
  SEXP canary = PROTECT(R_MakeExternalPtr(&canary_sentinel, R_NilValue, R_NilValue));
  R_SetExternalPtrProtected(canary, parent);
  R_RegisterCFinalizerEx(canary, canary_finalizer, TRUE);

  SEXP mdata = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(mdata, 0, canary);
  SET_VECTOR_ELT(mdata, 1, times);
  SET_VECTOR_ELT(mdata, 2, each);

  SEXP ans = R_new_altrep(cls, mdata, R_NilValue);
  UNPROTECT(2); /* canary, mdata */

  /* Propagate class/dim so R-level dispatch on the altrep works. */
  copy_vector_attrs(ans, parent);

  /* If parent has names, attach a vrep_str for them so that names() on the
     result returns the correctly-repeated names without materialising the
     parent data.  Names repeat with the same each/times as the data. */
  SEXP par_names = getAttrib(parent, R_NamesSymbol);
  if (par_names != R_NilValue) {
    SEXP rep_names = PROTECT(make_vrep_internal(par_names, times, each,
                                                rep_str_class));
    setAttrib(ans, R_NamesSymbol, rep_names);
    UNPROTECT(1);
  }

  return ans;
}

/* ── shared ALTREP methods ──────────────────────────────────────────────── */

static SEXP vrep_Serialized_state(SEXP x) {
  /* Force materialisation on serialise; no attempt to preserve ALTREP form. */
  return NULL;
}

static Rboolean vrep_Inspect(SEXP x, int pre, int deep, int pvec,
                              void (*inspect_subtree)(SEXP, int, int, int)) {
  const char *tname = type2char(TYPEOF(x));
  if (VREP_EXPANDED(x) != R_NilValue)
    Rprintf(" vrep<%s> [expanded]\n", tname);
  else
    Rprintf(" vrep<%s> [par %p pattern_len: %.0f each: %.0f times: %.0f]\n",
            tname, (void *)VREP_PARENT(x),
            (double)VREP_PATTERN_LEN(x),
            (double)VREP_EACH(x),
            (double)VREP_TIMES(x));
  return TRUE;
}

static R_xlen_t vrep_Length(SEXP x) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue)
    return XLENGTH(exp);
  return VREP_PATTERN_LEN(x) * VREP_EACH(x) * VREP_TIMES(x);
}

/* ── index mapping ──────────────────────────────────────────────────────── */

/* Maps result index i to the parent index, honouring both each and times. */
static R_INLINE R_xlen_t vrep_parent_idx(SEXP x, R_xlen_t i) {
  return (i / VREP_EACH(x)) % VREP_PATTERN_LEN(x);
}

/* ── materialisation helper ─────────────────────────────────────────────── */

/*
 * Expand the repetition into a freshly allocated vector of the same type as
 * the parent.  Copies class/dim attributes onto the result.
 * Returns the new SEXP (caller must UNPROTECT 1).
 */
static SEXP vrep_materialise(SEXP x) {
  SEXP parent   = VREP_PARENT(x);
  R_xlen_t len  = vrep_Length(x);
  SEXPTYPE tp   = TYPEOF(parent);

  SEXP ans = PROTECT(allocVector(tp, len));
  copy_vector_attrs(ans, parent);

  switch (tp) {
    case REALSXP: {
      const double *src = REAL_RO(parent);
      double       *dst = REAL(ans);
      for (R_xlen_t j = 0; j < len; j++) dst[j] = src[vrep_parent_idx(x, j)];
      break;
    }
    case INTSXP:
    case LGLSXP: {
      const int *src = INTEGER_RO(parent);
      int       *dst = INTEGER(ans);
      for (R_xlen_t j = 0; j < len; j++) dst[j] = src[vrep_parent_idx(x, j)];
      break;
    }
    case CPLXSXP: {
      const Rcomplex *src = COMPLEX_RO(parent);
      Rcomplex       *dst = COMPLEX(ans);
      for (R_xlen_t j = 0; j < len; j++) dst[j] = src[vrep_parent_idx(x, j)];
      break;
    }
    case RAWSXP: {
      const Rbyte *src = RAW_RO(parent);
      Rbyte       *dst = RAW(ans);
      for (R_xlen_t j = 0; j < len; j++) dst[j] = src[vrep_parent_idx(x, j)];
      break;
    }
    case STRSXP: {
      for (R_xlen_t j = 0; j < len; j++)
        SET_STRING_ELT(ans, j, STRING_ELT(parent, vrep_parent_idx(x, j)));
      break;
    }
    case VECSXP: {
      for (R_xlen_t j = 0; j < len; j++)
        SET_VECTOR_ELT(ans, j, VECTOR_ELT(parent, vrep_parent_idx(x, j)));
      break;
    }
    default:
      error("vrep_materialise: unsupported type %s", type2char(tp));
  }
  return ans; /* caller UNPROTECTs 1 */
}

/* ── ALTVEC Dataptr / Dataptr_or_null ───────────────────────────────────── */

/*
 * These are shared across all numeric/raw ALTREP classes (REAL/INT/LGL/CPLX/RAW).
 * STRSXP and VECSXP do not use Dataptr for element access, but R may still
 * call it in coercion paths, so we provide it for them too.
 */
static void *vrep_Dataptr(SEXP x, Rboolean writeable) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) {
    switch (TYPEOF(exp)) {
      case REALSXP: return REAL(exp);
      case INTSXP:
      case LGLSXP:  return INTEGER(exp);
      case CPLXSXP: return COMPLEX(exp);
      case RAWSXP:  return RAW(exp);
      default: error("vrep_Dataptr: no contiguous buffer for type '%s'",
                     type2char(TYPEOF(exp)));
    }
  }

  SEXP ans = vrep_materialise(x); /* PROTECT(1) inside */
  VREP_SET_EXPANDED(x, ans);
  VREP_UNSET_PARENT(x); /* release parent; no longer needed once materialised */
  UNPROTECT(1); /* ans — still reachable via data2 */

  switch (TYPEOF(ans)) {
    case REALSXP: return REAL(ans);
    case INTSXP:
    case LGLSXP:  return INTEGER(ans);
    case CPLXSXP: return COMPLEX(ans);
    case RAWSXP:  return RAW(ans);
    default: error("vrep_Dataptr: no contiguous buffer for type '%s'",
                   type2char(TYPEOF(ans)));
  }
}

static const void *vrep_Dataptr_or_null(SEXP x) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp == R_NilValue)
    return NULL; /* stay ALTREP */
  switch (TYPEOF(exp)) {
    case REALSXP: return REAL_RO(exp);
    case INTSXP:
    case LGLSXP:  return INTEGER_RO(exp);
    case CPLXSXP: return COMPLEX_RO(exp);
    case RAWSXP:  return RAW_RO(exp);
    default:      return NULL; /* STRSXP/VECSXP: no contiguous public buffer */
  }
}

/* ── ALTREAL ────────────────────────────────────────────────────────────── */

static double vrep_real_Elt(SEXP x, R_xlen_t i) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return REAL_ELT(exp, i);
  return REAL_ELT(VREP_PARENT(x), vrep_parent_idx(x, i));
}

static R_xlen_t vrep_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t n,
                                     double *buf) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return REAL_GET_REGION(exp, i, n, buf);
  R_xlen_t xlen  = vrep_Length(x);
  R_xlen_t ncopy = (xlen - i < n) ? xlen - i : n;
  const double *par = REAL_RO(VREP_PARENT(x));
  for (R_xlen_t j = 0; j < ncopy; j++) buf[j] = par[vrep_parent_idx(x, i + j)];
  return ncopy;
}

static int vrep_real_Is_sorted(SEXP x) {
  if (VREP_EXPANDED(x) != R_NilValue) return UNKNOWN_SORTEDNESS;
  R_xlen_t plen = VREP_PATTERN_LEN(x);
  if (plen == 1)          return SORTED_INCR;
  if (VREP_TIMES(x) > 1) return UNKNOWN_SORTEDNESS;
  return REAL_IS_SORTED(VREP_PARENT(x));
}

/* ── shared No_NA helper ────────────────────────────────────────────────── */

/*
 * If the vector has been expanded we conservatively return 0 — a writable
 * Dataptr may have introduced NAs.  Otherwise we delegate to the parent's
 * own No_NA query via the supplied callback so that ALTREP parents can
 * short-circuit without a full scan.
 */
static int vrep_No_NA(SEXP x, int (*parent_no_na)(SEXP)) {
  if (VREP_EXPANDED(x) != R_NilValue) return 0;
  return parent_no_na(VREP_PARENT(x));
}

static int vrep_real_No_NA(SEXP x) { return vrep_No_NA(x, REAL_NO_NA);    }

/* ── shared summary helper ──────────────────────────────────────────────── */

/*
 * Call fn(par, na.rm = narm) in R_BaseEnv and return the result.
 * Used by Min/Max/Sum methods to delegate to R's own generics, which will
 * themselves dispatch through the parent's ALTREP methods when present.
 */
static SEXP vrep_summary(const char *fn, SEXP par, Rboolean narm) {
  SEXP call = PROTECT(Rf_lang3(Rf_install(fn),
                                par,
                                Rf_ScalarLogical(narm)));
  /* na.rm is a named argument */
  SET_TAG(CDDR(call), Rf_install("na.rm"));
  SEXP res = Rf_eval(call, R_BaseEnv);
  UNPROTECT(1);
  return res;
}

static SEXP vrep_parent_or_expanded(SEXP x) {
  SEXP exp = VREP_EXPANDED(x);
  return exp != R_NilValue ? exp : VREP_PARENT(x);
}

/* ── ALTREAL ────────────────────────────────────────────────────────────── */

static SEXP vrep_real_Min(SEXP x, Rboolean narm) {
  return vrep_summary("min", vrep_parent_or_expanded(x), narm);
}

static SEXP vrep_real_Max(SEXP x, Rboolean narm) {
  return vrep_summary("max", vrep_parent_or_expanded(x), narm);
}

static SEXP vrep_real_Sum(SEXP x, Rboolean narm) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue)
    return vrep_summary("sum", exp, narm);
  SEXP s = PROTECT(vrep_summary("sum", VREP_PARENT(x), narm));
  double result = REAL_ELT(s, 0) * (double)(VREP_EACH(x) * VREP_TIMES(x));
  UNPROTECT(1);
  return Rf_ScalarReal(result);
}

/* Shared by vrep_int_Sum and vrep_lgl_Sum: scale an integer/logical sum. */
static SEXP vrep_intlgl_Sum(SEXP x, Rboolean narm) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue)
    return vrep_summary("sum", exp, narm);
  SEXP s = PROTECT(vrep_summary("sum", VREP_PARENT(x), narm));
  R_xlen_t scale = (R_xlen_t)VREP_EACH(x) * VREP_TIMES(x);
  SEXP result;
  if (TYPEOF(s) == INTSXP) {
    int sv = INTEGER_ELT(s, 0);
    if (sv == NA_INTEGER) {
      result = Rf_ScalarInteger(NA_INTEGER);
    } else {
      double dv = (double)sv * (double)scale;
      if (dv > INT_MAX || dv < INT_MIN) {
        Rf_warning("integer overflow in vrep sum; returning double");
        result = Rf_ScalarReal(dv);
      } else {
        result = Rf_ScalarInteger((int)dv);
      }
    }
  } else {
    result = Rf_ScalarReal(REAL_ELT(s, 0) * (double)scale);
  }
  UNPROTECT(1);
  return result;
}

/* ── ALTINTEGER (also covers LGLSXP via separate class) ─────────────────── */

static int vrep_int_Elt(SEXP x, R_xlen_t i) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return INTEGER_ELT(exp, i);
  return INTEGER_ELT(VREP_PARENT(x), vrep_parent_idx(x, i));
}

static R_xlen_t vrep_int_Get_region(SEXP x, R_xlen_t i, R_xlen_t n,
                                    int *buf) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return INTEGER_GET_REGION(exp, i, n, buf);
  R_xlen_t xlen  = vrep_Length(x);
  R_xlen_t ncopy = (xlen - i < n) ? xlen - i : n;
  const int *par = INTEGER_RO(VREP_PARENT(x));
  for (R_xlen_t j = 0; j < ncopy; j++) buf[j] = par[vrep_parent_idx(x, i + j)];
  return ncopy;
}

static int vrep_int_Is_sorted(SEXP x) {
  if (VREP_EXPANDED(x) != R_NilValue) return UNKNOWN_SORTEDNESS;
  R_xlen_t plen = VREP_PATTERN_LEN(x);
  if (plen == 1)          return SORTED_INCR;
  if (VREP_TIMES(x) > 1) return UNKNOWN_SORTEDNESS;
  return INTEGER_IS_SORTED(VREP_PARENT(x));
}

static SEXP vrep_int_Min(SEXP x, Rboolean narm) {
  return vrep_summary("min", vrep_parent_or_expanded(x), narm);
}

static SEXP vrep_int_Max(SEXP x, Rboolean narm) {
  return vrep_summary("max", vrep_parent_or_expanded(x), narm);
}

static SEXP vrep_int_Sum(SEXP x, Rboolean narm) {
  return vrep_intlgl_Sum(x, narm);
}

static int vrep_int_No_NA (SEXP x) { return vrep_No_NA(x, INTEGER_NO_NA); }

/* ── ALTLOGICAL ─────────────────────────────────────────────────────────── */

static int vrep_lgl_Elt(SEXP x, R_xlen_t i) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return LOGICAL_ELT(exp, i);
  return LOGICAL_ELT(VREP_PARENT(x), vrep_parent_idx(x, i));
}

static R_xlen_t vrep_lgl_Get_region(SEXP x, R_xlen_t i, R_xlen_t n,
                                    int *buf) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return LOGICAL_GET_REGION(exp, i, n, buf);
  R_xlen_t xlen  = vrep_Length(x);
  R_xlen_t ncopy = (xlen - i < n) ? xlen - i : n;
  const int *par = LOGICAL_RO(VREP_PARENT(x));
  for (R_xlen_t j = 0; j < ncopy; j++) buf[j] = par[vrep_parent_idx(x, i + j)];
  return ncopy;
}

static SEXP vrep_lgl_Sum(SEXP x, Rboolean narm) {
  return vrep_intlgl_Sum(x, narm);
}

static int vrep_lgl_No_NA (SEXP x) { return vrep_No_NA(x, LOGICAL_NO_NA); }

/* ── ALTCOMPLEX ─────────────────────────────────────────────────────────── */

static Rcomplex vrep_cplx_Elt(SEXP x, R_xlen_t i) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return COMPLEX_ELT(exp, i);
  return COMPLEX_ELT(VREP_PARENT(x), vrep_parent_idx(x, i));
}

static R_xlen_t vrep_cplx_Get_region(SEXP x, R_xlen_t i, R_xlen_t n,
                                     Rcomplex *buf) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return COMPLEX_GET_REGION(exp, i, n, buf);
  R_xlen_t xlen  = vrep_Length(x);
  R_xlen_t ncopy = (xlen - i < n) ? xlen - i : n;
  const Rcomplex *par = COMPLEX_RO(VREP_PARENT(x));
  for (R_xlen_t j = 0; j < ncopy; j++) buf[j] = par[vrep_parent_idx(x, i + j)];
  return ncopy;
}

/* ── ALTRAW ─────────────────────────────────────────────────────────────── */

static Rbyte vrep_raw_Elt(SEXP x, R_xlen_t i) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return RAW_ELT(exp, i);
  return RAW_ELT(VREP_PARENT(x), vrep_parent_idx(x, i));
}

/* No Get_region API for ALTRAW in current R headers. */

/* ── ALTSTRING ──────────────────────────────────────────────────────────── */

static SEXP vrep_str_Elt(SEXP x, R_xlen_t i) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return STRING_ELT(exp, i);
  return STRING_ELT(VREP_PARENT(x), vrep_parent_idx(x, i));
}

static void vrep_str_Set_elt(SEXP x, R_xlen_t i, SEXP v) {
  /* Setting an element forces full materialisation first. */
  SEXP exp = VREP_EXPANDED(x);
  if (exp == R_NilValue) {
    SEXP mat = vrep_materialise(x); /* PROTECT(1) */
    VREP_SET_EXPANDED(x, mat);
    VREP_UNSET_PARENT(x);
    UNPROTECT(1);
    exp = VREP_EXPANDED(x);
  }
  SET_STRING_ELT(exp, i, v);
}

static int vrep_str_Is_sorted(SEXP x) {
  if (VREP_EXPANDED(x) != R_NilValue) return UNKNOWN_SORTEDNESS;
  R_xlen_t plen = VREP_PATTERN_LEN(x);
  if (plen == 1)          return SORTED_INCR;
  if (VREP_TIMES(x) > 1) return UNKNOWN_SORTEDNESS;
  return STRING_IS_SORTED(VREP_PARENT(x));
}
static int vrep_str_No_NA (SEXP x) { return 0; } /* no STRING_NO_NA in public API */

/* ── ALTLIST ────────────────────────────────────────────────────────────── */

static SEXP vrep_list_Elt(SEXP x, R_xlen_t i) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp != R_NilValue) return VECTOR_ELT(exp, i);
  return VECTOR_ELT(VREP_PARENT(x), vrep_parent_idx(x, i));
}

static void vrep_list_Set_elt(SEXP x, R_xlen_t i, SEXP v) {
  SEXP exp = VREP_EXPANDED(x);
  if (exp == R_NilValue) {
    SEXP mat = vrep_materialise(x); /* PROTECT(1) */
    VREP_SET_EXPANDED(x, mat);
    VREP_UNSET_PARENT(x);
    UNPROTECT(1);
    exp = VREP_EXPANDED(x);
  }
  SET_VECTOR_ELT(exp, i, v);
}

/* ── class initialisers ─────────────────────────────────────────────────── */

static void InitVRepRealClass(DllInfo *dll) {
  R_altrep_class_t cls = R_make_altreal_class("vrep_real", "vecrep", dll);
  rep_real_class = cls;

  R_set_altrep_Inspect_method(cls, vrep_Inspect);
  R_set_altrep_Length_method(cls, vrep_Length);
  R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

  R_set_altvec_Dataptr_method(cls, vrep_Dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, vrep_Dataptr_or_null);

  R_set_altreal_Elt_method(cls, vrep_real_Elt);
  R_set_altreal_Get_region_method(cls, vrep_real_Get_region);
  R_set_altreal_Is_sorted_method(cls, vrep_real_Is_sorted);
  R_set_altreal_No_NA_method(cls, vrep_real_No_NA);
  R_set_altreal_Min_method(cls, vrep_real_Min);
  R_set_altreal_Max_method(cls, vrep_real_Max);
  R_set_altreal_Sum_method(cls, vrep_real_Sum);
}

static void InitVRepIntClass(DllInfo *dll) {
  R_altrep_class_t cls = R_make_altinteger_class("vrep_int", "vecrep", dll);
  rep_int_class = cls;

  R_set_altrep_Inspect_method(cls, vrep_Inspect);
  R_set_altrep_Length_method(cls, vrep_Length);
  R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

  R_set_altvec_Dataptr_method(cls, vrep_Dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, vrep_Dataptr_or_null);

  R_set_altinteger_Elt_method(cls, vrep_int_Elt);
  R_set_altinteger_Get_region_method(cls, vrep_int_Get_region);
  R_set_altinteger_Is_sorted_method(cls, vrep_int_Is_sorted);
  R_set_altinteger_Min_method(cls, vrep_int_Min);
  R_set_altinteger_Max_method(cls, vrep_int_Max);
  R_set_altinteger_Sum_method(cls, vrep_int_Sum);
  R_set_altinteger_No_NA_method(cls, vrep_int_No_NA);
}

static void InitVRepLglClass(DllInfo *dll) {
  R_altrep_class_t cls = R_make_altlogical_class("vrep_lgl", "vecrep", dll);
  rep_lgl_class = cls;

  R_set_altrep_Inspect_method(cls, vrep_Inspect);
  R_set_altrep_Length_method(cls, vrep_Length);
  R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

  R_set_altvec_Dataptr_method(cls, vrep_Dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, vrep_Dataptr_or_null);

  R_set_altlogical_Elt_method(cls, vrep_lgl_Elt);
  R_set_altlogical_Get_region_method(cls, vrep_lgl_Get_region);
  R_set_altlogical_Sum_method(cls, vrep_lgl_Sum);
  R_set_altlogical_No_NA_method(cls, vrep_lgl_No_NA);
}

static void InitVRepCplxClass(DllInfo *dll) {
  R_altrep_class_t cls = R_make_altcomplex_class("vrep_cplx", "vecrep", dll);
  rep_cplx_class = cls;

  R_set_altrep_Inspect_method(cls, vrep_Inspect);
  R_set_altrep_Length_method(cls, vrep_Length);
  R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

  R_set_altvec_Dataptr_method(cls, vrep_Dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, vrep_Dataptr_or_null);

  R_set_altcomplex_Elt_method(cls, vrep_cplx_Elt);
  R_set_altcomplex_Get_region_method(cls, vrep_cplx_Get_region);
}

static void InitVRepRawClass(DllInfo *dll) {
  R_altrep_class_t cls = R_make_altraw_class("vrep_raw", "vecrep", dll);
  rep_raw_class = cls;

  R_set_altrep_Inspect_method(cls, vrep_Inspect);
  R_set_altrep_Length_method(cls, vrep_Length);
  R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

  R_set_altvec_Dataptr_method(cls, vrep_Dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, vrep_Dataptr_or_null);

  R_set_altraw_Elt_method(cls, vrep_raw_Elt);
}

static void InitVRepStrClass(DllInfo *dll) {
  R_altrep_class_t cls = R_make_altstring_class("vrep_str", "vecrep", dll);
  rep_str_class = cls;

  R_set_altrep_Inspect_method(cls, vrep_Inspect);
  R_set_altrep_Length_method(cls, vrep_Length);
  R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

  /* No Dataptr: STRSXP has no contiguous data buffer. */

  R_set_altstring_Elt_method(cls, vrep_str_Elt);
  R_set_altstring_Set_elt_method(cls, vrep_str_Set_elt);
  R_set_altstring_Is_sorted_method(cls, vrep_str_Is_sorted);
  R_set_altstring_No_NA_method(cls, vrep_str_No_NA);
}

static void InitVRepListClass(DllInfo *dll) {
  R_altrep_class_t cls = R_make_altlist_class("vrep_list", "vecrep", dll);
  rep_list_class = cls;

  R_set_altrep_Inspect_method(cls, vrep_Inspect);
  R_set_altrep_Length_method(cls, vrep_Length);
  R_set_altrep_Serialized_state_method(cls, vrep_Serialized_state);

  /* No Dataptr: VECSXP has no contiguous data buffer. */

  R_set_altlist_Elt_method(cls, vrep_list_Elt);
  R_set_altlist_Set_elt_method(cls, vrep_list_Set_elt);
}

/* ── public constructor ─────────────────────────────────────────────────── */

/*
 * make_vrep(parent, times, each)
 *
 * Dispatches on typeof(parent) to pick the right ALTREP class.
 * Both `times` and `each` must be length-1 positive integer scalars.
 *
 * Equivalent to base::rep(parent, times = times, each = each).
 */
SEXP make_vrep(SEXP parent, SEXP times, SEXP each) {
  /* Normalise times to a length-1 INTSXP, then validate. */
  if (TYPEOF(times) != INTSXP || XLENGTH(times) != 1)
    times = PROTECT(coerceVector(times, INTSXP));
  else
    PROTECT(times);
  if (INTEGER_ELT(times, 0) == NA_INTEGER || INTEGER_ELT(times, 0) < 1)
    error("make_vrep: 'times' must be a positive integer");
  UNPROTECT(1);

  /* Normalise each to a length-1 INTSXP, then validate. */
  if (TYPEOF(each) != INTSXP || XLENGTH(each) != 1)
    each = PROTECT(coerceVector(each, INTSXP));
  else
    PROTECT(each);
  if (INTEGER_ELT(each, 0) == NA_INTEGER || INTEGER_ELT(each, 0) < 1)
    error("make_vrep: 'each' must be a positive integer");
  UNPROTECT(1);

  R_altrep_class_t cls;
  switch (TYPEOF(parent)) {
    case REALSXP: cls = rep_real_class;  break;
    case INTSXP:  cls = rep_int_class;   break;
    case LGLSXP:  cls = rep_lgl_class;   break;
    case CPLXSXP: cls = rep_cplx_class;  break;
    case RAWSXP:  cls = rep_raw_class;   break;
    case STRSXP:  cls = rep_str_class;   break;
    case VECSXP:  cls = rep_list_class;  break;
    default:
      error("make_vrep: unsupported type '%s'", type2char(TYPEOF(parent)));
  }

  return make_vrep_internal(parent, times, each, cls);
}

/* Keep the old entry point as a thin wrapper for backward compatibility. */
SEXP make_rep_real(SEXP parent, SEXP times) {
  if (TYPEOF(parent) != REALSXP)
    error("make_rep_real: parent must be a numeric (double) vector");
  SEXP each = PROTECT(ScalarInteger(1));
  SEXP ans  = make_vrep(parent, times, each);
  UNPROTECT(1);
  return ans;
}

/* ── DLL registration ───────────────────────────────────────────────────── */

static const R_CallMethodDef CallEntries[] = {
    {"make_vrep",     (DL_FUNC)&make_vrep,     3},
    {"make_rep_real", (DL_FUNC)&make_rep_real, 2},
    {NULL, NULL, 0}
};

void R_init_vecrep(DllInfo *dll) {
  InitVRepRealClass(dll);
  InitVRepIntClass(dll);
  InitVRepLglClass(dll);
  InitVRepCplxClass(dll);
  InitVRepRawClass(dll);
  InitVRepStrClass(dll);
  InitVRepListClass(dll);

  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
