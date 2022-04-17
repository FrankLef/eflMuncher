#' Flag the data lines outside the fence
#'
#' prune data outside the fence.
#'
#' Prune data within the outside the fence. Rows with \code{prune_id} not NA
#' or with non-finite values will be excluded from the fence.
#' Normally, data should be pruned in the following order
#'  - non-finite: Lines with \code{Inf} and \code{NA}
#'  - Out-of-bound: Lines outside of reasonable range set by user
#'  - Fence: Lines outside the fences delimited by the slopes
#'           \code{min(x) / max(y)} and \code{max(x) / min(y)}.
#'           See details and the source.
#' \emph{Important:} Fencing is not always a good thing. For example with a data
#'                  set where the data is contains in an elongated area, e.g.
#'                  the iris data set for Sepal.Width and Petal.Width.
#'
#' @section The Fence:
#'
#' The fence is an algebraic way of testing the proposition
#' if x > A then y > 0 AND if y > B then x > 0. See the 2 tests with plots to
#' illustrate in test scripts.
#'
#' @param df dataframe
#' @param x String, name of the numeric column "X" to test
#' @param y String, name of the numeric column "Y" to test.
#' @param prune_var String, name of the column with the prune identifications
#' @param id String, identification of the pruning in \code{prune_var}
#' @param details Flag, TRUE return a list with extra info on slopes and
#'  data supporting the fence calculations. Default is \code{FALSE}.
#'
#' @return dataframe with an extra flag column
#' @export
#'
#' @importFrom rlang .data :=
#' @importFrom dplyr %>% mutate between near
#'
#' @source Statistical Data Cleaning by Mark van der Loo and Edwin de Jonge
#'  in section 7.5.2, p. 177-179.
#'
#' @examples
#' df <- data.frame(
#'  valX = c(0.0, 0.0, 0.3, 1.0, 3.0, 3.0, 3.0, 7.5, 8.0, 8.2),
#'  valY = c(0.0, 0.5, 6.0, 2.0, 0.0, 2.5, 8.1, 1.0, 7.0, 6.0),
#'  prune_id = factor(NA_character_)
#'  )
#' df <- prune_fence(df, x = "valX", y = "valY")
#' # This should give pruned 3 points
#' stopifnot(sum(!is.na(df$prune_id)) == 3)
prune_fence <- function(df, x, y, prune_var = "prune_id",
                        id = "fence", details = FALSE) {
  checkmate::assertDataFrame(df, min.rows = 3)
  checkmate::assertNames(names(df), must.include = c(x, y, prune_var))
  checkmate::assertString(id, min.chars = 1)
  checkmate::assertFlag(details)


  # the flag must be reset
  df <- prune_reset(df, prune_var = prune_var, id = id)

  # all values must be finite
  check <- df %>%
    dplyr::filter(is.na(.data[[prune_var]]),
                  !is.finite(.data[[x]]) | !is.finite(.data[[y]]))
  check <- nrow(check)
  if (!check) {
    # add the new factor, forcats::fct_expand() will, silently,
    # add it if it already exist
    df <- df %>%
      mutate(!!prune_var := forcats::fct_expand(.data[[prune_var]],id))
  } else {
    msg_head <- cli::col_yellow("All values must be finite to prune.")
    msg_body <- sprintf("Prune %s or %s for non-finite values", x, y)
    msg_body <- c("i" = msg_body,
                  "i" = sprintf("Nb of rows with non-finite values = %d", check))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_error1")
  }


  # split based on the prune id
  lst <- split(x = df, f = is.na(df[, prune_var]))
  # cat("\n", crayon::bold$yellow(paste("inside:", "lst")), "\n")
  # print(nrow(lst[["TRUE"]]))

  if(!length(lst[["TRUE"]])) {
    msg_head <- cli::col_yellow("There are no row to prune.")
    msg_body <- c("i" = "There is no prune id with NA. Verify.")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_error2")
  }

  # when a preexisting prune-id cause the split to be in noncontiguous
  # part, a problem occurs with duplicate rownames when unplit()
  # therefoe we keep rnames to reapply them just before unsplit()
  # NOTE: Use attr(), not row.names(), row.names changes the mode
  rnames <- attr(lst[["TRUE"]], which = "row.names")

  # compute the fences' specs
  the_specs <- prune_fence_slopes(x = lst[["TRUE"]][, x],
                                  y = lst[["TRUE"]][, y])


  # cat("\n", crayon::bold$yellow(paste("inside:", "work")), "\n")
  # print(work)

  fence <- prune_fence_df(df = lst[["TRUE"]], x = x, y = y, specs = the_specs)

  lst[["TRUE"]] <- prune_upd(lst[["TRUE"]], cond = fence$flag,
                             prune_var = prune_var, id = id)

  # reassign rownames to avoid error about duplicate rownames
  attr(lst[["TRUE"]], which = "row.names") <- rnames
  # unsplit the dataframe
  df <- unsplit(value = lst, f = is.na(df[, prune_var]))
  # assertthat::assert_that(sum(!is.na(df[, prune_var])) >= sum(fence$flag))

  if (!details) {
    out <- df
  } else {
    out <- list("df" = df, "fence_df" = fence, "specs" = the_specs)
  }
  out
}



#' Compute the fences' slope
#'
#' Compute the fences' slope
#'
#' Compute the fences' slopes.
#'
#' @param x Numeric with only finite values
#' @param y Numeric with only finite values
#'
#'
#' @return Numeric vectors with the 2 slopes
prune_fence_slopes <- function(x, y) {
  checkmate::assertNumeric(x, finite = TRUE, any.missing = FALSE, min.len = 3)
  checkmate::assertNumeric(y, finite = TRUE, any.missing = FALSE, len = length(x))

  msg <- "All values in x and/or y are the same. Fencing won't work."
  assertthat::assert_that(length(unique(x)) > 1, length(unique(y)) > 1,
                          msg = msg)

  # must only used values > 0 to compute the slope
  x_pos <- x[x > 0]
  y_pos <- y[y > 0]
  assertthat::assert_that(all(x_pos > 0), all(y_pos > 0))
  # cat("\n", crayon::bold$yellow(paste("inside:", "x_pos")), "\n")
  # print(x_pos)
  # cat("\n", crayon::bold$yellow(paste("inside:", "y_pos")), "\n")
  # print(y_pos)

  # the slope used with a given x to determine the limit above which y must be
  low_slope = min(x_pos) / max(y_pos)
  # the slope used with a given x to determine the limit below which y must be
  high_slope = max(x_pos) / min(y_pos)

  # the slope will be equal only when min(x) == max(x) and min(x) == max(y)
  # that is, when all data points are equal which is a very unusual event.
  if (near(low_slope, high_slope, tol = .Machine$double.eps^0.5)) {
    msg_head <- sprintf("The fence slopes are nearly equal.")
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- "This can only happen if all SCALED POSITIVE values are equal."
    msg_body <- c("i" = msg_body,
                  "i" = sprintf("Low slope = %f", low_slope),
                  "i" = sprintf("High slope = %f", high_slope))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_calc_error1")
  } else if (low_slope > high_slope) {
    msg_head <- sprintf("The low slope must be less than the high slope.")
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- c("X" = "Verify the max and min of the SCALED POSITIVE values.",
                  "i" = sprintf("Low slope = %f", low_slope),
                  "i" = sprintf("High slope = %f", high_slope))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_calc_error2")
  } else if(low_slope < 0 | high_slope < 0) {
    msg_head <- sprintf("At least one of the 2 fenconce slope is negative.")
    msg_head <- cli::col_yellow(msg_head)
    msg_body <- c("X" = "Verify the max and min of the SCALED POSITIVE values.",
                  "i" = sprintf("Low slope = %f", low_slope),
                  "i" = sprintf("High slope = %f", high_slope))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_fence_calc_error3")
  }

  c("low_slope" = low_slope, "high_slope" = high_slope)
}


#' Update the fence data for pruning
#'
#' Update the fence data for pruning
#'
#' Update the fence data for pruning using the slope calculated with
#' \code{prune_fence_slopes}
#'
#' @param df Dataframe
#' @param x String, name of x variable
#' @param y String, name of y variable
#' @param specs Numeric vector of fence slopes and scale constant
#'
#' @importFrom rlang .data :=
#' @importFrom dplyr %>% select
#'
#' @return Dataframe with fence results
prune_fence_df <- function(df, x, y, specs) {
  checkmate::assertDataFrame(df, min.rows = 3)
  checkmate::assertNames(names(df), must.include = c(x, y))
  checkmate::assertNumeric(specs, len = 2, finite = TRUE, any.missing = FALSE)

  msg <- sprintf("The %s column includes non-finite values", x)
  assertthat::assert_that(all(is.finite(df[, x])), msg = msg)
  msg <- sprintf("The %s column includes non-finite values", y)
  assertthat::assert_that(all(is.finite(df[, y])), msg = msg)

  # the scale constant
  low_slope <- specs["low_slope"]
  high_slope <- specs["high_slope"]

  # create the fence dataframe
  fence <- df %>%
    dplyr::select(.data[[x]], .data[[y]])

  # the fence values
  fence$low <- fence[, x] * low_slope
  fence$high <- fence[, x] * high_slope

  # update the fence flag
  fence$flag <- FALSE
  fence$flag <- fence$flag | fence[, y] < fence$low
  fence$flag <- fence$flag | fence[, y] > fence$high

  fence
}
