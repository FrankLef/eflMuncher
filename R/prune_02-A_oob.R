#' Flag Rows Outside of a Given Range
#'
#' @inheritParams prune
#' @param rng The range of numerical values. The values outside of \code{rng}
#' will be identified with TRUE.
#'
#' @seealso prune_proc
#'
#' @return Logical vector where TRUE indicates values outside of \code{rng}.
#' @export
prune_oob_num <- function(data, cols, rng = c(-Inf, Inf)) {
  checkmate::assertDataFrame(data, min.rows = 1, min.cols = 1)
  checkmate::assertNames(cols, subset.of = names(data))
  checkmate::assertNumeric(rng, any.missing = FALSE, len = 2, sorted = TRUE)

  # verify the range
  assertthat::assert_that(abs(diff(rng)) > .Machine$double.eps^0.5)

  df <- data[, cols]
  # test each column
  out <- apply(X = df, MARGIN = 2, FUN = function(x) {
    x < rng[1] | x > rng[2]
  })
  # find all rows where any value is TRUE
  sel <- apply(out, MARGIN = 1, FUN = function(x) {
    any(x)
  })
  sel
}
