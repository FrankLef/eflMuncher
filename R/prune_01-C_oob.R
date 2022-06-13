#' Prune Rows with Values Outside a Given Range
#'
#' Prune rows with values outside a given range.
#'
#' Flag the data lines with numeric values outside of the allowed
#' range set by the user. The "flag" column \code{prune_var} will be
#' updated to TRUE when the value tested is outside the range. The NA and Inf
#' values are treated as outside the range.
#'
#' @param df dataframe
#' @param var String, name of the numeric column to prune
#' @param prune_var String, name of the column with the prune id
#' @param id String, identification of the pruning in \code{prune_var}
#' @param rng Numeric, The range of values
#'
#' @return dataframe with updated \code{prune_var}  column
#' @export
#'
#' @importFrom rlang .data :=
#' @importFrom dplyr %>% mutate between
#'
#' @examples
#' rng <- c(4, 7)
#' df <- data.frame(iris, prune_id = factor(NA_character_))
#' df <- prune_oob(df, var = "Sepal.Length", rng = rng)
#' # This should give 12 pruned points with prune id == "range"
#' check <- sum(iris$Sepal.Length < rng[1] | iris$Sepal.Length > rng[2])
#' stopifnot(sum(!is.na(df$prune_id)) == check)
prune_oob <- function(df, var, prune_var = "prune_id", id = "oob",
                        rng = c(-Inf, Inf)) {
  checkmate::assertNames(names(df), must.include = c(var, prune_var))
  checkmate::assertString(id, min.chars = 1)
  checkmate::assertNumeric(rng, len = 2, any.missing = FALSE,
                           sorted = TRUE, unique = TRUE)

  # the range must be wide enough
  check <- near(diff(rng), 0L, tol = .Machine$double.eps^0.5)
  if (check) {
    msg_head <- cli::col_yellow("The min and max are nearly identical:")
    msg_body <- c("i" = "The data cannot be binned when the range is too small.",
                  "i" = "Verify if all values are finite?")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_range_error1")
  }

  # the flag must be reset
  df <- prune_reset(df, prune_var = prune_var, id = id)

  # all values must be finite
  check <- df %>%
    dplyr::filter(is.na(.data[[prune_var]]), !is.finite(.data[[var]]))
  check <- nrow(check)
  if (check){
    msg_head <- cli::col_yellow("All values to prune must be finite.")
    msg_body <- c("i" = "Prune the non-finite values before using this function.",
                  "i" = sprintf("Nb of non-finite values = %d", check))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_range_error2")
  }

  # split based on the prune id
  lst <- split(x = df, f = is.na(df[, prune_var]))

  if(!length(lst[["TRUE"]])) {
    msg_head <- cli::col_yellow("There are no row to prune.")
    msg_body <- c("i" = "There is no prune id with NA. Verify.")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_range_error3")
  }

  # when a preexisting prune-id cause the split to be in noncontiguous
  # part, a problem occurs with duplicate rownames when unplit()
  # therefoe we keep rnames to reapply them just before unsplit()
  rnames <- attr(lst[["TRUE"]], which = "row.names")

  # update the pruning identification
  cond <- !dplyr::between(lst[["TRUE"]][, var], rng[1], rng[2])
  lst[["TRUE"]] <- prune_upd(df = lst[["TRUE"]], cond = cond,
                             prune_var = prune_var, id = id)

  # reassign rownames to avoid error about duplicate rownames
  attr(lst[["TRUE"]], which = "row.names") <- rnames
  # unsplit the dataframe
  df <- unsplit(value = lst, f = is.na(df[, prune_var]))
  df
}
