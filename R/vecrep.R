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
#' @param x A vector to repeat. Must be one of: double, integer, logical,
#'   complex, raw, character, or list (including classed variants thereof).
#' @param times A single positive integer giving the number of times to repeat
#'   `x`.
#'
#' @return An ALTREP vector of the same type and class as `x`, with length
#'   `length(x) * times`.
#'
#' @examples
#' rep_altrep(rnorm(5), 3)
#' rep_altrep(1L:4L, 2L)
#' rep_altrep(c(TRUE, FALSE, NA), 4L)
#' rep_altrep(factor(c("a", "b", "c")), 3L)
#' rep_altrep(as.Date("2024-01-01") + 0:2, 2L)
#' rep_altrep(c("foo", "bar"), 5L)
#' rep_altrep(list(1, "a", TRUE), 3L)
#'
#' @export
rep_altrep <- function(x, times) {
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
  times <- as.integer(times)
  .Call(C_make_vrep, x, times)
}
