# ---------------------------------------------------------------------------
# ALTREP inspection helpers
# ---------------------------------------------------------------------------

# Returns TRUE when x is a vrep_real ALTREP that has NOT been expanded.
# inspect() line: "rep double [par <addr> pattern_len: N times: M]"
is_vrep_altrep <- function(x) {
  out <- utils::capture.output(.Internal(inspect(x)))
  any(grepl("rep double \\[par", out))
}

# Returns TRUE when the vrep_real ALTREP shell exists but data2 is expanded.
# inspect() line: "rep double [ expanded ]"
is_vrep_expanded <- function(x) {
  out <- utils::capture.output(.Internal(inspect(x)))
  any(grepl("rep double \\[ expanded \\]", out))
}

# Returns TRUE when x is ALTREP (either unexpanded or expanded vrep shell).
is_vrep <- function(x) is_vrep_altrep(x) || is_vrep_expanded(x)

# Returns TRUE when x has been materialised — either the ALTREP shell has
# expanded its data2 buffer, OR copy-on-write replaced it with a plain vector.
# A plain allocated REALSXP has "(len=N, tl=N)" in inspect output.
is_materialised <- function(x) {
  is_vrep_expanded(x) ||
    any(grepl("\\(len=", utils::capture.output(.Internal(inspect(x)))))
}

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

small_parent <- as.numeric(1:10)
na_parent <- c(1.0, NA_real_, 3.0)

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("rep_altrep rejects integer parent (fails at C level)", {
  # mode(1L:5L) == "numeric" passes the R stopifnot, but REAL() in C rejects integers.
  expect_error(rep_altrep(1L:5L, 2L))
})

test_that("rep_altrep rejects character parent", {
  expect_error(rep_altrep(letters[1:5], 2L), "double")
})

test_that("rep_altrep rejects list parent", {
  expect_error(rep_altrep(list(1, 2), 2L), "double")
})

test_that("rep_altrep rejects non-scalar times", {
  expect_error(rep_altrep(small_parent, c(2L, 3L)), "length")
})

test_that("rep_altrep rejects times < 1", {
  expect_error(rep_altrep(small_parent, 0L), "times")
})

# ---------------------------------------------------------------------------
# Basic correctness — output matches base::rep()
# ---------------------------------------------------------------------------

test_that("rep_altrep matches rep() for times = 1", {
  y <- rep_altrep(small_parent, 1L)
  expect_equal(as.numeric(y), rep(small_parent, 1L))
})

test_that("rep_altrep matches rep() for times = 3", {
  y <- rep_altrep(small_parent, 3L)
  expect_equal(as.numeric(y), rep(small_parent, 3L))
})

test_that("rep_altrep matches rep() for times = 10", {
  y <- rep_altrep(small_parent, 10L)
  expect_equal(as.numeric(y), rep(small_parent, 10L))
})

test_that("rep_altrep handles a single-element parent", {
  y <- rep_altrep(42.0, 5L)
  expect_equal(as.numeric(y), rep(42.0, 5L))
})

test_that("rep_altrep preserves NA values correctly", {
  y <- rep_altrep(na_parent, 2L)
  expect_equal(as.numeric(y), rep(na_parent, 2L))
})

# ---------------------------------------------------------------------------
# Length and type
# ---------------------------------------------------------------------------

test_that("rep_altrep returns correct length", {
  expect_length(rep_altrep(small_parent, 1L), 10L)
  expect_length(rep_altrep(small_parent, 3L), 30L)
  expect_length(rep_altrep(small_parent, 10L), 100L)
})

test_that("rep_altrep result has mode numeric", {
  expect_equal(mode(rep_altrep(small_parent, 2L)), "numeric")
})

# ---------------------------------------------------------------------------
# ALTREP status — freshly created objects must be ALTREP
# ---------------------------------------------------------------------------

test_that("freshly created rep_altrep is an unexpanded vrep_real ALTREP", {
  y <- rep_altrep(small_parent, 3L)
  expect_true(is_vrep_altrep(y))
  expect_false(is_vrep_expanded(y))
  expect_false(is_materialised(y))
})

test_that("base::rep() result is NOT a vrep_real ALTREP", {
  expect_false(is_vrep_altrep(rep(small_parent, 3L)))
})

# ---------------------------------------------------------------------------
# Element-level access stays lazy (no materialisation)
# ---------------------------------------------------------------------------

test_that("reading a single element does not materialise", {
  y <- rep_altrep(small_parent, 3L)
  val <- y[5]
  expect_false(is_materialised(y))
  expect_equal(val, small_parent[5])
})

test_that("element access wraps correctly across repetition boundaries", {
  y <- rep_altrep(small_parent, 3L)
  expect_equal(y[11], small_parent[1])
  expect_equal(y[15], small_parent[5])
  expect_equal(y[25], small_parent[5])
})

test_that("sum() does not materialise", {
  y <- rep_altrep(small_parent, 3L)
  s <- sum(y)
  expect_false(is_materialised(y))
  expect_equal(s, sum(small_parent) * 3L)
})

test_that("mean() does not materialise", {
  y <- rep_altrep(small_parent, 3L)
  m <- mean(y)
  expect_false(is_materialised(y))
  expect_equal(m, mean(small_parent))
})

# ---------------------------------------------------------------------------
# Materialisation — write path must expand the data buffer
# ---------------------------------------------------------------------------

test_that("sort() expands data2 but retains vrep shell", {
  y <- rep_altrep(small_parent, 3L)
  sort(y)
  expect_false(is_vrep_altrep(y))
  expect_true(is_vrep_expanded(y))
  expect_true(is_materialised(y))
})

test_that("after sort()-induced materialisation, values are still correct", {
  y <- rep_altrep(small_parent, 3L)
  sort(y)
  expect_equal(as.numeric(y), rep(small_parent, 3L))
})

test_that("[<- replaces binding with a plain vector", {
  # [<- triggers copy-on-write, replacing the binding with an ordinary REALSXP
  y <- rep_altrep(small_parent, 3L)
  y[1] <- 999.0
  expect_true(is_vrep(y))
  expect_true(is_materialised(y))
})

test_that("after [<-, values are correct", {
  y <- rep_altrep(small_parent, 3L)
  expected <- rep(small_parent, 3L)
  expected[1] <- 999.0
  y[1] <- 999.0
  expect_equal(as.numeric(y), expected)
})

test_that("[<- does not modify the parent vector", {
  parent <- as.numeric(1:5)
  y <- rep_altrep(parent, 2L)
  y[1] <- -1.0
  expect_equal(parent[1], 1.0)
})

# ---------------------------------------------------------------------------
# No-NA hint propagation
# ---------------------------------------------------------------------------

test_that("No_NA hint is TRUE when parent has no NAs", {
  y <- rep_altrep(as.numeric(1:5), 2L)
  expect_false(anyNA(y))
})

test_that("No_NA hint is FALSE (conservative) when parent contains NA", {
  y <- rep_altrep(na_parent, 2L)
  expect_true(anyNA(y))
})
