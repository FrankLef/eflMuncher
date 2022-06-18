#' Identify the Rows Outside the Fence.
#'
#' Identify the rows outside the fence.
#'
#' Compute the fence to eliminate values that are clearly out-of-bound.
#' Normally all values should be non-negative. In case they are not, and
#' offset is used. Also, sometimes the data is nowhere near zero and is such
#' cases the fence is not useful, again in that case the offset solves that
#' problem. The algorithm will generate an error when \code{-Inf, Inf} values
#' are in the input. The \code{NA} are treated as being out-of-bound.
#'
#' @section info:
#'
#' if the argument \code{info} is set to \code{TRUE} then a list with the following elements is given.
#' \describe{
#'   \item{is_outside}{Logical vector, TRUE is when the row is outside the
#'   fence limits, FALSE otherwise.}
#'   \item{slopes}{The slopes for the small and big fences.}
#'   \item{offsets}{The offset used to scale the x and y values.}
#'   \item{fences}{Dataframe with x = original x values; y = original y values,
#'   small = y value of the small fence on the scaled coordinates;
#'   big = y value of the big fence on the scaled coordinates;
#'   small_inv = y value of the small fence on the original scale
#'   (useful for plotting);
#'   big_inv = y value of the big fence on the original scale
#'   (useful for plotting.)}
#' }
#'
#' @inheritParams prune
#' @param is_offset If TRUE (default) the offset number will be
#' \code{offset = min(min(x), min(y))}, otherwise there will be not offset,
#' that is \code{offset = 0}.
#' @param info If FALSE (default) a logical vector with the is returned. If TRUE
#' a list with the logical vector is returned as well as the slopes. This is
#' used usually to help plot the fences.
#'
#' @source \emph{Statistical Data Cleaning}, Mark van der Loo and
#' Edwin de Jonge, 2018. Section 7.5.2, p. 176-179.
#'
#' @return If \code{info = FALSE} (default), logical vector where TRUE indicates
#' values outside the fence. If \code{info = TRUE}, a list with
#' the logical vector called \code{is_outside}, the list of slopes called
#' \code{slopes}, the list of offsets called \code{offsets}, and the data.frame
#' of fences data \code{fences}.
#' @export
prune_fence <- function(data, cols, is_offset = TRUE, info = FALSE) {
  checkmate::assertDataFrame(data, min.rows = 2, min.cols = 2)
  checkmate::assertCharacter(cols, min.chars = 1, len = 2, unique = TRUE,
                             any.missing = FALSE)
  checkmate::assertNames(cols, subset.of = names(data))
  checkmate::assertNumeric(data[, cols[1]], finite = TRUE)
  checkmate::assertNumeric(data[, cols[2]], finite = TRUE)
  checkmate::assertFlag(is_offset)

  x <- data[, cols[1]]
  y <- data[, cols[2]]

  # the data is scaled to the minimum of all data.
  # This normally increases the chances of the algorithm to work.
  # Some cases don't work when not using an offset.
  # See the test when is_offset = FALSE
  if (is_offset) {
    # the_offset = min(min(x, na.rm = TRUE), min(y, na.rm = TRUE))
    offsets <- list("x" = min(x, na.rm = TRUE), "y" = min(y, na.rm = TRUE))
  } else {
    # the_offset <- 0
    offsets <- list("x" = 0, "y" = 0)
  }
  # cat("\n", "offset", "\n")
  # print(the_offset)
  # cat("\n")
  # assertthat::assert_that(is.finite(the_offset))
  assertthat::assert_that(is.finite(offsets$x), is.finite(offsets$y))

  # scale x and y with offset
  x_scaled <- x - offsets$x
  y_scaled <- y - offsets$y
  # cat("\n", "x_scaled", "\n")
  # print(x_scaled)
  # cat("\n", "y_scaled", "\n")
  # print(y_scaled)
  # all values must be >= 0 and finite
  checkmate::assertNumeric(x_scaled, finite = TRUE, lower = 0)
  checkmate::assertNumeric(y_scaled, finite = TRUE, lower = 0)

  # get the slopes
  slopes <- prune_fence_slopes(x_scaled, y_scaled)

  # the fence values
  fences <- data.frame("x" = x,
                       "y" = y,
                       "small" = x_scaled * slopes$small,
                       "big" = x_scaled * slopes$big)
  # add the computation of the fences on the original scale,
  # that is we invert to return to th original scale
  fences$small_inv <- fences$small + offsets$y
  fences$big_inv <- fences$big + offsets$y

  # all rows with NA are considered outside the fences
  is_outside <- is.na(x_scaled) | is.na(y_scaled)

  # flag values outside the fences
  is_outside <- is_outside | y_scaled < fences$small
  is_outside <- is_outside | y_scaled > fences$big

  # return results depending on info
  if(!info) {
    out <- is_outside
  } else {
    out <- list("is_outside" = is_outside,
                "slopes" = slopes,
                "offsets" = offsets,
                "fences" = fences)
  }
  out
}


#' Compute the Slopes Used for Fencing
#'
#' Compute the slopes used for fencing.
#'
#' The function compute the slopes and also applies several tests to the result.
#' Problem with the data will always usually affect the ratio.
#'
#' @param x Numeric vector. Must have finite values.
#' @param y Numeric vector. Must have finite values.
#' @param tol Number, tolerance when testing the ratios.
#'
#' @return List with \code{small_slope} and \code{big_slope}.
#' @export
prune_fence_slopes <- function(x, y, tol = .Machine$double.eps^0.5) {
  checkmate::assertNumeric(x, finite = TRUE, min.len = 3)
  checkmate::assertNumeric(y, finite = TRUE, len = length(x))
  checkmate::assertNumber(tol, lower = 0, finite = TRUE)

  # must only used values positive values to compute the slope
  # and make sure all NA are excluded
  x_pos <- x[x > 0 & !is.na(x)]
  y_pos <- y[y > 0 & !is.na(y)]
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
  small_slope = min(x_pos, na.rm = TRUE) / max(y_pos, na.rm = TRUE)
  # the slope used with a given x to determine the limit below which y must be
  big_slope = max(x_pos, na.rm = TRUE) / min(y_pos, na.rm = TRUE)
  assertthat::assert_that(small_slope > 0, big_slope > 0)
  # cat("\n", "inside: the slopes", "\n")
  # print(sprintf("small slope = %f, big slope = %f", small_slope, big_slope))
  # cat("\n")


  x_rng <- range(x_pos)
  y_rng <- range(y_pos)
  if (diff(x_rng) < tol | diff(y_rng) < tol) {
    # Fencing does not work when at least one of the 2 data vectors
    # has constantv values
    msg_head <- "All pos values in x or y are about the same. Fencing won't work."
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- "This happen if at least one of the 2 columns has constant data."
    msg_body <- c("X" = msg_body,
                  "i" = sprintf("x pos value range: %s", toString(x_rng)),
                  "i" = sprintf("y pos value range: %s", toString(y_rng)))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_slopes_error1")
   }

  list("small" = small_slope, "big" = big_slope)
}
