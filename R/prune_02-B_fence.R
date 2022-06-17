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
  # all values must be >= 0
  checkmate::assertNumeric(x, finite = TRUE, lower = 0)
  checkmate::assertNumeric(y, finite = TRUE, lower = 0)

  # get the slopes
  slopes <- prune_fence_slopes(x, y)


}


prune_fence_slopes <- function(x, y) {
  checkmate::assertNumeric(x, finite = TRUE, any.missing = FALSE, min.len = 3)
  checkmate::assertNumeric(y, finite = TRUE, any.missing = FALSE, len = length(x))

  # must only used values > 0 to compute the slope
  x_pos <- x[x > 0]
  y_pos <- y[y > 0]
  assertthat::assert_that(length(x_pos) > 0,
                          msg = "There are no positive values in x.")
  assertthat::assert_that(length(y_pos) > 0,
                          msg = "There are no positive values in y.")
  # cat("\n", crayon::bold$yellow(paste("inside:", "x_pos")), "\n")
  # print(x_pos)
  # cat("\n", crayon::bold$yellow(paste("inside:", "y_pos")), "\n")
  # print(y_pos)

  # the slope used with a given x to determine the limit above which y must be
  small_slope = min(x_pos) / max(y_pos)
  # the slope used with a given x to determine the limit below which y must be
  big_slope = max(x_pos) / min(y_pos)


  if (length(unique(x_pos)) == 1 | length(unique(y_pos)) == 1) {
    # Fencing does not work when at least one of the 2 data vectors
    # has constantv alues
    msg_head <- sprintf("All values in x and/or y are the same. Fencing won't work.")
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- "This happen if at least one of the 2 columns has constant data."
    msg_body <- c("i" = msg_body,
                  "i" = sprintf("Mean x value = %f", mean(x_pos)),
                  "i" = sprintf("Mean y value = %f", mean(y_pos)))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_slopes_error1")

   } else if (small_slope >= big_slope) {
    msg_head <- sprintf("The low slope must be less than the high slope.")
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- c("X" = "Verify the max and min of the SCALED POSITIVE values.",
                  "i" = sprintf("Small slope = %f", small_slope),
                  "i" = sprintf("Big slope = %f", big_slope))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_slopes_error2")
  } else if (small_slope < .Machine$double.eps^0.5 |
             big_slope < .Machine$double.eps^0.5) {
    msg_head <- sprintf("At least one of the 2 slopes is negative.")
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- c("X" = "Verify the max and min of the SCALED POSITIVE values.",
                  "i" = sprintf("Small slope = %f", small_slope),
                  "i" = sprintf("Big slope = %f", big_slope))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_slopes_error3")
  }

  list("small" = small_slope, "big" = big_slope)
}
