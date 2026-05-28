#' Repeat a vector using ALTREP
#'
#' Creates a repeated vector backed by an ALTREP representation,
#' avoiding materialisation of the full vector in memory until necessary.
#'
#' Supported types: double, integer, logical, complex, raw, character, and list.
#' Classed vectors (e.g. `factor`, `Date`, `POSIXct`) are handled
#' transparently: the class attribute is preserved on the returned object so
#' S3 dispatch continues to work without forcing materialisation.
#'
#' `times` and `each` can be combined freely, matching the behaviour of
#' [base::rep()]: `each` replicates individual elements first, then `times`
#' repeats the resulting pattern.  Providing only `times` is equivalent to
#' `rep(x, times = times)`; providing only `each` is equivalent to
#' `rep(x, each = each)`.
#'
#' @param x A vector to repeat. Must be one of: double, integer, logical,
#'   complex, raw, character, or list (including classed variants thereof).
#' @param times A single positive integer giving the number of times to repeat
#'   the whole (each-expanded) pattern. Defaults to `1L`.
#' @param each A single positive integer giving the number of times each
#'   element is repeated before moving to the next. Defaults to `1L`.
#'
#' @return An ALTREP vector of the same type and class as `x`, with length
#'   `length(x) * each * times`.
#'
#' @examples
#' rep_altrep(letters[1:4], times = 2)
#' rep_altrep(letters[1:4], each = 2)
#' rep_altrep(letters[1:4], times = 2, each = 3)
#' rep_altrep(1L:4L, each = 2L)
#' rep_altrep(c(TRUE, FALSE, NA), each = 2L, times = 3L)
#' rep_altrep(factor(c("a", "b", "c")), each = 2L)
#' rep_altrep(as.Date("2024-01-01") + 0:2, each = 2L)
#' rep_altrep(c("foo", "bar"), times = 5L)
#' rep_altrep(list(1, "a", TRUE), each = 2L, times = 2L)
#'
#' @export
rep_altrep <- function(x, times = 1L, each = 1L) {
  .supported <- c("double", "integer", "logical", "complex", "raw",
                  "character", "list")
  if (!typeof(x) %in% .supported) {
    stop(sprintf(
      "`x` must be a vector with typeof() in {%s}; got '%s'",
      paste(.supported, collapse = ", "),
      typeof(x)
    ))
  }
  stopifnot(length(times) == 1L, !is.na(times), times >= 1L)
  stopifnot(length(each)  == 1L, !is.na(each),  each  >= 1L)
  times <- as.integer(times)
  each  <- as.integer(each)
  .Call(C_make_vrep, x, times, each)
}
