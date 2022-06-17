#' Identify the Rows Outside the Fence.
#'
#' Identify the rows outside the fence.
#'
#' Compute the fence to eliminate values that are clearly out-of-bound.
#' Normally all values should be nonnegative. In case they are not, and
#' offset is used. Also, sometimes the data is nowhere near zero and is such
#' cases the fence is not useful, again in that case the offset solves that
#' problem.
#'
#' @inheritParams prune
#' @param is_offset If TRUE (default) the offset number will be
#' \code{offset = min(min(x), min(y))}, otherwise there will be not offset,
#' that is \code{offset = 0}.
#'
#' @source \emph{Statistical Data Cleaning}, Mark van der Loo and
#' Edwin de Jonge, 2018. Section 7.5.2, p. 176-179.
#'
#' @return Logical vector where TRUE indicates values outside the fence.
#' @export
prune_fence <- function(data, cols, is_offset = TRUE) {
  checkmate::assertDataFrame(data, min.rows = 2, min.cols = 2)
  checkmate::assertCharacter(cols, min.chars = 1, len = 2, unique = TRUE,
                             any.missing = FALSE)
  checkmate::assertNames(cols, subset.of = names(data))
  checkmate::assertNumeric(data[, cols[1]], any.missing = FALSE, finite = TRUE)
  checkmate::assertNumeric(data[, cols[2]], any.missing = FALSE, finite = TRUE)
  checkmate::assertFlag(is_offset)


  if (is_offset) {
    the_offset = min(min(data[, cols[1]]), min(data[, cols[2]]), na.rm = TRUE)
  } else {
    the_offset <- 0
  }
  assertthat::assert_that(is.finite(the_offset))

  # compute x and y with offset
  x <- data[, cols[1]] - the_offset
  y <- data[, cols[2]] - the_offset
  # all values must be >= 0 and finite
  checkmate::assertNumeric(x, finite = TRUE, lower = 0)
  checkmate::assertNumeric(y, finite = TRUE, lower = 0)

  # get the slopes
  slopes <- prune_fence_slopes(x, y)


}


#' Compute the Slopes Used for Fencing
#'
#' Compute the slopes used for fencing.
#'
#' The function compute the slopes and also applies several tests to the result.
#' Problem with the data will always usually affect the ratio.
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param tol Number, tolerance when testing the ratios.
#'
#' @return List with \code{small_slope} and \code{big_slope}.
#' @export
prune_fence_slopes <- function(x, y, tol = .Machine$double.eps^0.5) {
  checkmate::assertNumeric(x, finite = TRUE, any.missing = FALSE, min.len = 3)
  checkmate::assertNumeric(y, finite = TRUE, any.missing = FALSE, len = length(x))
  checkmate::assertNumber(tol, lower = 0, finite = TRUE)

  # must only used values positive values to compute the slope
  x_pos <- x[x > 0]
  y_pos <- y[y > 0]
  msg <- sprintf("There are only %d positive values in scaled x.", length(x_pos))
  assertthat::assert_that(length(x_pos) >= 2, msg = msg)
  msg <- sprintf("There are only %d positive values in scaled y.", length(y_pos))
  assertthat::assert_that(length(y_pos) >= 2, msg = msg)
  # cat("\n", "inside: x_pos", "\n")
  # msg <- sprintf("Range of x_pos: %s", toString(range(x_pos)))
  # print(msg)
  # cat("inside: y_pos", "\n")
  # msg <- sprintf("Range of y_pos: %s", toString(range(y_pos)))
  # print(msg)
  # cat("\n")

  # the slope used with a given x to determine the limit above which y must be
  small_slope = min(x_pos) / max(y_pos)
  # the slope used with a given x to determine the limit below which y must be
  big_slope = max(x_pos) / min(y_pos)
  assertthat::assert_that(small_slope > 0, big_slope > 0)
  # cat("\n", "inside: the slopes", "\n")
  # print(sprintf("small slope = %f, big slope = %f", small_slope, big_slope))
  # cat("\n")


  if (abs(diff(range(x_pos))) < tol | abs(diff(range(y_pos))) < tol) {
    # Fencing does not work when at least one of the 2 data vectors
    # has constantv values
    msg_head <- "All pos values in x or y are about the same. Fencing won't work."
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- "This happen if at least one of the 2 columns has constant data."
    msg_body <- c("X" = msg_body,
                  "i" = sprintf("x pos value range: %s",
                                toString(range(x_pos))),
                  "i" = sprintf("y pos value range: %s",
                                toString(range(y_pos))))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_slopes_error1")

   }

  list("small" = small_slope, "big" = big_slope)
}
