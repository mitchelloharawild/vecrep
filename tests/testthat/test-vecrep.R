# ---------------------------------------------------------------------------
# ALTREP inspection helpers
# ---------------------------------------------------------------------------

# The inspect output for an compact vrep object looks like:
#   vrep<double> [par <addr> pattern_len: N times: M]
# and for an expanded one:
#   vrep<double> [expanded]

is_vrep_compact <- function(x) {
  out <- utils::capture.output(.Internal(inspect(x)))
  any(grepl("vrep<[a-z]+> \\[par", out))
}

is_vrep_expanded <- function(x) {
  out <- utils::capture.output(.Internal(inspect(x)))
  any(grepl("vrep<[a-z]+> \\[expanded\\]", out))
}

is_vrep <- function(x) is_vrep_compact(x) || is_vrep_expanded(x)

is_materialised <- function(x) {
  out <- utils::capture.output(.Internal(inspect(x)))
  # Only inspect the first line (the vrep node itself) to avoid false positives
  # from attribute SEXPs — e.g. a "class" STRSXP has "(len=1, tl=0)" in its
  # inspect line, which must not be confused with an expanded data buffer.
  out <- utils::capture.output(.Internal(inspect(x)))[[1L]]
  is_vrep_expanded(x) || grepl("\\(len=", out)
}

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

dbl_par  <- c(1.1, 2.2, 3.3, 4.4, 5.5)
int_par  <- 1L:5L
lgl_par  <- c(TRUE, FALSE, NA)
cplx_par <- c(1+2i, 3+4i, 5+6i)
raw_par  <- as.raw(c(0x01, 0x02, 0x03))
chr_par  <- c("foo", "bar", "baz")
lst_par  <- list(1L, "a", TRUE)

fct_par  <- factor(c("cat", "dog", "cat"))
dat_par  <- as.Date("2024-01-01") + 0:2
pct_par  <- as.POSIXct("2024-01-01") + 0:2

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("rep_altrep rejects non-vector types", {
  expect_error(rep_altrep(NULL,         2L))
  expect_error(rep_altrep(quote(x),     2L))
  expect_error(rep_altrep(new.env(),    2L))
})

test_that("rep_altrep rejects non-scalar times", {
  expect_error(rep_altrep(dbl_par, c(2L, 3L)), "length")
})

test_that("rep_altrep rejects times < 1", {
  expect_error(rep_altrep(dbl_par, 0L))
  expect_error(rep_altrep(dbl_par, -1L))
})

test_that("rep_altrep rejects NA times", {
  expect_error(rep_altrep(dbl_par, NA_integer_))
})

# ---------------------------------------------------------------------------
# Helpers shared across per-type blocks
# ---------------------------------------------------------------------------

check_type <- function(result, expected_typeof) {
  expect_equal(typeof(result), expected_typeof)
}

check_matches_base_rep <- function(par, times) {
  result   <- rep_altrep(par, times)
  expected <- rep(par, times)
  # Compare underlying data, bypassing class for typed vectors
  expect_equal(unclass(result), unclass(expected))
}

check_altrep <- function(par, times = 3L) {
  y <- rep_altrep(par, times)
  expect_true(is_vrep_compact(y))
  expect_false(is_materialised(y))
}

check_elt_access_altrep <- function(par, times = 3L) {
  y   <- rep_altrep(par, times)
  val <- y[[2L]]
  expect_false(is_materialised(y))
  expect_equal(val, par[[2L]])
}

check_boundary_wrap <- function(par, times = 3L) {
  n  <- length(par)
  y  <- rep_altrep(par, times)
  # Element at position n+1 must equal element 1 of parent
  expect_equal(y[[n + 1L]], par[[1L]])
}

# ---------------------------------------------------------------------------
# double (REALSXP)
# ---------------------------------------------------------------------------

test_that("double: is altrep ALTREP on creation", {
  check_altrep(dbl_par)
})

test_that("double: typeof is double", {
  check_type(rep_altrep(dbl_par, 2L), "double")
})

test_that("double: length is correct", {
  expect_length(rep_altrep(dbl_par, 3L), 15L)
})

test_that("double: matches base::rep()", {
  check_matches_base_rep(dbl_par, 4L)
})

test_that("double: element access stays altrep", {
  check_elt_access_altrep(dbl_par)
})

test_that("double: wraps correctly at repetition boundaries", {
  check_boundary_wrap(dbl_par)
})

test_that("double: NA values are preserved", {
  par <- c(1.0, NA_real_, 3.0)
  expect_equal(unclass(rep_altrep(par, 2L)), rep(par, 2L))
})

test_that("double: No_NA hint TRUE when no NAs in parent", {
  expect_false(anyNA(rep_altrep(dbl_par, 3L)))
})

test_that("double: No_NA hint FALSE when parent has NA", {
  expect_true(anyNA(rep_altrep(c(1.0, NA_real_), 2L)))
})

# ---------------------------------------------------------------------------
# integer (INTSXP)
# ---------------------------------------------------------------------------

test_that("integer: is altrep ALTREP on creation", {
  check_altrep(int_par)
})

test_that("integer: typeof is integer", {
  check_type(rep_altrep(int_par, 2L), "integer")
})

test_that("integer: length is correct", {
  expect_length(rep_altrep(int_par, 3L), 15L)
})

test_that("integer: matches base::rep()", {
  check_matches_base_rep(int_par, 4L)
})

test_that("integer: element access stays altrep", {
  check_elt_access_altrep(int_par)
})

test_that("integer: wraps correctly at repetition boundaries", {
  check_boundary_wrap(int_par)
})

test_that("integer: NA values are preserved", {
  par <- c(1L, NA_integer_, 3L)
  expect_equal(unclass(rep_altrep(par, 2L)), rep(par, 2L))
})

# ---------------------------------------------------------------------------
# logical (LGLSXP)
# ---------------------------------------------------------------------------

test_that("logical: is altrep ALTREP on creation", {
  check_altrep(lgl_par)
})

test_that("logical: typeof is logical", {
  check_type(rep_altrep(lgl_par, 2L), "logical")
})

test_that("logical: length is correct", {
  expect_length(rep_altrep(lgl_par, 4L), 12L)
})

test_that("logical: matches base::rep()", {
  check_matches_base_rep(lgl_par, 3L)
})

test_that("logical: element access stays altrep", {
  check_elt_access_altrep(lgl_par)
})

test_that("logical: NA preserved", {
  y <- rep_altrep(lgl_par, 2L)
  expect_true(is.na(y[[3L]]))
  expect_true(is.na(y[[6L]]))
})

# ---------------------------------------------------------------------------
# complex (CPLXSXP)
# ---------------------------------------------------------------------------

test_that("complex: is altrep ALTREP on creation", {
  check_altrep(cplx_par)
})

test_that("complex: typeof is complex", {
  check_type(rep_altrep(cplx_par, 2L), "complex")
})

test_that("complex: length is correct", {
  expect_length(rep_altrep(cplx_par, 3L), 9L)
})

test_that("complex: matches base::rep()", {
  check_matches_base_rep(cplx_par, 3L)
})

test_that("complex: element access stays altrep", {
  check_elt_access_altrep(cplx_par)
})

# ---------------------------------------------------------------------------
# raw (RAWSXP)
# ---------------------------------------------------------------------------

test_that("raw: is altrep ALTREP on creation", {
  check_altrep(raw_par)
})

test_that("raw: typeof is raw", {
  check_type(rep_altrep(raw_par, 2L), "raw")
})

test_that("raw: length is correct", {
  expect_length(rep_altrep(raw_par, 4L), 12L)
})

test_that("raw: matches base::rep()", {
  check_matches_base_rep(raw_par, 3L)
})

test_that("raw: element access stays altrep", {
  check_elt_access_altrep(raw_par)
})

# ---------------------------------------------------------------------------
# character (STRSXP)
# ---------------------------------------------------------------------------

test_that("character: is altrep ALTREP on creation", {
  check_altrep(chr_par)
})

test_that("character: typeof is character", {
  check_type(rep_altrep(chr_par, 2L), "character")
})

test_that("character: length is correct", {
  expect_length(rep_altrep(chr_par, 3L), 9L)
})

test_that("character: matches base::rep()", {
  check_matches_base_rep(chr_par, 3L)
})

test_that("character: element access stays altrep", {
  check_elt_access_altrep(chr_par)
})

test_that("character: wraps correctly at boundaries", {
  check_boundary_wrap(chr_par)
})

test_that("character: NA_character_ preserved", {
  par <- c("a", NA_character_, "c")
  y   <- rep_altrep(par, 2L)
  expect_true(is.na(y[[2L]]))
  expect_true(is.na(y[[5L]]))
})

test_that("character: Set_elt materialises and updates correctly", {
  y        <- rep_altrep(chr_par, 2L)
  y[[1L]]  <- "REPLACED"
  expected <- rep(chr_par, 2L)
  expected[[1L]] <- "REPLACED"
  expect_equal(y, expected)
})

# ---------------------------------------------------------------------------
# list (VECSXP)
# ---------------------------------------------------------------------------

test_that("list: is altrep ALTREP on creation", {
  check_altrep(lst_par)
})

test_that("list: typeof is list", {
  check_type(rep_altrep(lst_par, 2L), "list")
})

test_that("list: length is correct", {
  expect_length(rep_altrep(lst_par, 3L), 9L)
})

test_that("list: matches base::rep()", {
  check_matches_base_rep(lst_par, 3L)
})

test_that("list: element access stays altrep", {
  y   <- rep_altrep(lst_par, 3L)
  val <- y[[2L]]
  expect_false(is_materialised(y))
  expect_equal(val, lst_par[[2L]])
})

test_that("list: Set_elt materialises and updates correctly", {
  y        <- rep_altrep(lst_par, 2L)
  y[[1L]]  <- list("NEW")
  expected <- rep(lst_par, 2L)
  expected[[1L]] <- list("NEW")
  expect_equal(y, expected)
})

# ---------------------------------------------------------------------------
# Classed vectors: factor
# ---------------------------------------------------------------------------

test_that("factor: is altrep ALTREP on creation", {
  check_altrep(fct_par)
})

test_that("factor: class is preserved without materialisation", {
  y <- rep_altrep(fct_par, 3L)
  expect_s3_class(y, "factor")
  expect_false(is_materialised(y))
})

test_that("factor: levels are preserved without materialisation", {
  y <- rep_altrep(fct_par, 3L)
  expect_equal(levels(y), levels(fct_par))
  expect_false(is_materialised(y))
})

test_that("factor: matches base::rep()", {
  y <- rep_altrep(fct_par, 3L)
  expect_equal(unclass(y), unclass(rep(fct_par, 3L)))
  expect_equal(levels(y), levels(fct_par))
})

test_that("factor: as.character() gives correct strings", {
  y <- rep_altrep(fct_par, 2L)
  expect_equal(as.character(y), rep(as.character(fct_par), 2L))
})

test_that("factor: table() gives correct counts without prior materialisation", {
  y <- rep_altrep(fct_par, 4L)
  tb <- table(y)
  expect_equal(as.integer(tb[["cat"]]), 8L)
  expect_equal(as.integer(tb[["dog"]]), 4L)
})

# ---------------------------------------------------------------------------
# Classed vectors: Date
# ---------------------------------------------------------------------------

test_that("Date: is altrep ALTREP on creation", {
  check_altrep(dat_par)
})

test_that("Date: class is preserved without materialisation", {
  y <- rep_altrep(dat_par, 3L)
  expect_s3_class(y, "Date")
  expect_false(is_materialised(y))
})

test_that("Date: matches base::rep()", {
  y <- rep_altrep(dat_par, 3L)
  expect_equal(unclass(y), unclass(rep(dat_par, 3L)))
})

test_that("Date: format() gives correct strings", {
  y <- rep_altrep(dat_par, 2L)
  expect_equal(format(y), rep(format(dat_par), 2L))
})

# ---------------------------------------------------------------------------
# Classed vectors: POSIXct
# ---------------------------------------------------------------------------

test_that("POSIXct: is altrep ALTREP on creation", {
  check_altrep(pct_par)
})

test_that("POSIXct: class is preserved without materialisation", {
  y <- rep_altrep(pct_par, 2L)
  expect_s3_class(y, "POSIXct")
  expect_false(is_materialised(y))
})

test_that("POSIXct: matches base::rep()", {
  y <- rep_altrep(pct_par, 2L)
  expect_equal(unclass(y), unclass(rep(pct_par, 2L)))
})

# ---------------------------------------------------------------------------
# Named vectors
# ---------------------------------------------------------------------------

test_that("names are altrep vrep on the altrep (not eagerly expanded)", {
  par <- c(a = 1.0, b = 2.0, c = 3.0)
  y   <- rep_altrep(par, 2L)
  expect_true(is_vrep_compact(y))
  # The names attribute is itself an compact vrep_str
  expect_true(is_vrep_compact(names(y)))
})

test_that("names match base::rep() without materialising parent data", {
  par <- c(a = 1.0, b = 2.0, c = 3.0)
  y   <- rep_altrep(par, 2L)
  expect_equal(names(y), names(rep(par, 2L)))
  # Parent data itself must still be compact
  expect_true(is_vrep_compact(y))
})

test_that("names are correct for integer parent", {
  par <- c(x = 1L, y = 2L, z = 3L)
  y   <- rep_altrep(par, 3L)
  expect_equal(names(y), names(rep(par, 3L)))
})

test_that("unnamed parent has NULL names", {
  y <- rep_altrep(dbl_par, 2L)
  expect_null(names(y))
})

# ---------------------------------------------------------------------------
# Materialisation via Dataptr (sort, c, etc.)
# ---------------------------------------------------------------------------

test_that("sort() expands data2 and retains vrep shell", {
  y <- rep_altrep(dbl_par, 3L)
  sort(y)
  expect_true(is_vrep_expanded(y))
})

test_that("after sort()-triggered materialisation, values are still correct", {
  y <- rep_altrep(dbl_par, 3L)
  sort(y)
  expect_equal(as.numeric(y), rep(dbl_par, 3L))
})

test_that("[<- on double triggers materialisation with correct values", {
  y        <- rep_altrep(dbl_par, 2L)
  expected <- rep(dbl_par, 2L)
  y[[1L]]  <- 999.0
  expected[[1L]] <- 999.0
  expect_equal(as.numeric(y), expected)
})

test_that("[<- does not modify parent vector", {
  par     <- c(1.0, 2.0, 3.0)
  y       <- rep_altrep(par, 2L)
  y[[1L]] <- -1.0
  expect_equal(par[[1L]], 1.0)
})

# ---------------------------------------------------------------------------
# each= parameter
# ---------------------------------------------------------------------------

check_matches_base_rep_each <- function(par, each, times = 1L) {
  result   <- rep_altrep(par, times = times, each = each)
  expected <- rep(par, times = times, each = each)
  expect_equal(unclass(result), unclass(expected))
}

test_that("each: double elements repeated in place", {
  check_matches_base_rep_each(dbl_par, each = 2L)
})

test_that("each: integer elements repeated in place", {
  check_matches_base_rep_each(int_par, each = 3L)
})

test_that("each: logical elements repeated in place", {
  check_matches_base_rep_each(lgl_par, each = 4L)
})

test_that("each: complex elements repeated in place", {
  check_matches_base_rep_each(cplx_par, each = 2L)
})

test_that("each: raw elements repeated in place", {
  check_matches_base_rep_each(raw_par, each = 3L)
})

test_that("each: character elements repeated in place", {
  check_matches_base_rep_each(chr_par, each = 2L)
})

test_that("each: list elements repeated in place", {
  check_matches_base_rep_each(lst_par, each = 2L)
})

test_that("each: factor elements repeated in place", {
  result   <- rep_altrep(fct_par, each = 2L)
  expected <- rep(fct_par, each = 2L)
  expect_equal(unclass(result), unclass(expected))
  expect_equal(levels(result), levels(expected))
})

test_that("each: length is plen * each", {
  expect_length(rep_altrep(dbl_par, each = 3L), length(dbl_par) * 3L)
})

test_that("each: stays ALTREP on creation", {
  expect_true(is_vrep_compact(rep_altrep(chr_par, each = 2L)))
})

test_that("each: element access stays ALTREP", {
  y <- rep_altrep(dbl_par, each = 3L)
  expect_equal(y[[1L]], dbl_par[[1L]])
  expect_equal(y[[2L]], dbl_par[[1L]])  # second copy of first element
  expect_equal(y[[3L]], dbl_par[[1L]])  # third copy of first element
  expect_equal(y[[4L]], dbl_par[[2L]])  # first copy of second element
  expect_false(is_materialised(y))
})

test_that("each = 1 is identical to default (times-only)", {
  y1 <- rep_altrep(dbl_par, times = 3L)
  y2 <- rep_altrep(dbl_par, times = 3L, each = 1L)
  expect_equal(as.numeric(y1), as.numeric(y2))
})

# ---------------------------------------------------------------------------
# each + times combined
# ---------------------------------------------------------------------------

test_that("each + times: double matches base::rep()", {
  check_matches_base_rep_each(dbl_par, each = 2L, times = 3L)
})

test_that("each + times: integer matches base::rep()", {
  check_matches_base_rep_each(int_par, each = 3L, times = 2L)
})

test_that("each + times: character matches base::rep()", {
  check_matches_base_rep_each(chr_par, each = 2L, times = 2L)
})

test_that("each + times: matches letters example from base::rep()", {
  result   <- rep_altrep(letters, each = 2L, times = 2L)
  expected <- rep(letters, each = 2L, times = 2L)
  expect_equal(result, expected)
})

test_that("each + times: length is plen * each * times", {
  n     <- length(dbl_par)
  each  <- 2L
  times <- 3L
  expect_length(rep_altrep(dbl_par, times = times, each = each),
                n * each * times)
})

test_that("each + times: stays ALTREP on creation", {
  expect_true(is_vrep_compact(rep_altrep(int_par, times = 2L, each = 3L)))
})

test_that("each + times: named vector names match base::rep()", {
  par <- c(a = 1.0, b = 2.0, c = 3.0)
  y   <- rep_altrep(par, times = 2L, each = 2L)
  expect_equal(names(y), names(rep(par, times = 2L, each = 2L)))
  expect_true(is_vrep_compact(y))
  expect_true(is_vrep_compact(names(y)))
})

test_that("each + times: materialisation produces correct values", {
  y        <- rep_altrep(dbl_par, times = 2L, each = 3L)
  expected <- rep(dbl_par, times = 2L, each = 3L)
  sort(y)  # triggers Dataptr -> materialise
  expect_true(is_vrep_expanded(y))
  expect_equal(as.numeric(y), expected)
})

# ---------------------------------------------------------------------------
# each= input validation
# ---------------------------------------------------------------------------

test_that("rep_altrep rejects each < 1", {
  expect_error(rep_altrep(dbl_par, each = 0L))
  expect_error(rep_altrep(dbl_par, each = -1L))
})

test_that("rep_altrep rejects NA each", {
  expect_error(rep_altrep(dbl_par, each = NA_integer_))
})

test_that("rep_altrep rejects non-scalar each", {
  expect_error(rep_altrep(dbl_par, each = c(2L, 3L)), "length")
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("times = 1 returns ALTREP with same values as parent", {
  for (par in list(dbl_par, int_par, lgl_par, chr_par, lst_par, fct_par)) {
    y <- rep_altrep(par, 1L)
    expect_true(is_vrep_compact(y))
    expect_equal(unclass(y), unclass(par))
  }
})

test_that("single-element parent repeats correctly", {
  expect_equal(as.numeric(rep_altrep(42.0, 5L)), rep(42.0, 5L))
  expect_equal(as.integer(rep_altrep(7L,   5L)), rep(7L, 5L))
})

test_that("large repetition stays altrep", {
  x <- seq_len(1e5L)
  z <- rep_altrep(x, 1000L)
  expect_true(is_vrep_compact(z))
  expect_length(z, 1e8L)
})
