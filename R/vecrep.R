
#' Repeat a numeric vector using ALTREP
#'
#' Creates a repeated numeric vector backed by an ALTREP representation,
#' avoiding materialisation of the full vector in memory.
#'
#' @param x A numeric vector to repeat.
#' @param times A single positive integer giving the number of times to repeat `x`.
#'
#' @return An ALTREP numeric vector of length `length(x) * times`.
#'
#' @examples
#' rep_altrep(rnorm(5), 3)
#'
#' @export
rep_altrep <- function(x, times) {
    stopifnot(is.double(x), length(times) == 1L, times >= 1L)
    times <- as.integer(times)
    .Call(C_make_rep_real, x, times)
}
