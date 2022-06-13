#' Prune Rows That are Not Finite
#'
#' Prune rows that are not finite.
#'
#' Prune \code{NA}, \code{NaN} and \code{Inf}. You can select which one by
#' using the flags \code{is_na} and \code{is_fin}, \code{is_na} will prune
#' the \code{NA} and \code{NaN} values. \code{is_fin} will prune \code{Inf}
#' and \code{-Inf}.
#'
#' \emph{Important}: This funciton, contrary to the other prune_X() functions
#' will not into account the existing id in \code{prune_var}.  It will override
#' everything.
#'
#' @param df dataframe
#' @param var String, name of the numeric column to prune
#' @param prune_var String, name of the column with the prune id
#' @param id String, identification of the pruning in \code{prune_var}
#' @param is_na TRUE: flag \code{NA} and \code{NaN}. default is \code{TRUE}
#' @param is_inf TRUE: flag \code{Inf} and \code{-Inf}. default is \code{TRUE}
#'
#' @return dataframe with updated \code{prune_var}  column.
#' @export
#'
#' @importFrom rlang .data :=
#' @importFrom dplyr %>% mutate between near
#'
#' @examples
#' df <- data.frame(
#'  alpha = 1:5,
#'  beta = c(1, NA_real_, NaN, -Inf, Inf),
#'  prune_id = factor(NA_character_))
#' df <- prune_notfin(df, var = "beta", prune_var = "prune_id", id = "notfin")
#' # this should give 4 pruning id
#' stopifnot(sum(!is.na(df$prune_id)) == 4)
prune_notfin <- function(df, var, prune_var = "prune_id", id = "notfin",
                         is_na = TRUE, is_inf = TRUE) {
  checkmate::assertNames(names(df), must.include = c(prune_var))
  checkmate::assertString(id, min.chars = 1)
  checkmate::assertFlag(is_na)
  checkmate::assertFlag(is_inf)

  # # the id must be in the levels
  if (!(id %in% levels(df[, prune_var]))) {
    df <- df %>%
      mutate(!!prune_var := forcats::fct_expand(f = .data[[prune_var]], id))
  }

  # prune NA and NaN
  if (is_na) {
    df <- df %>%
        mutate(!!prune_var := replace(x = .data[[prune_var]],
                                      list = is.na(.data[[var]]),
                                      values = id))
  }

  # prune Inf
  if (is_inf) {
      df <- df %>%
        mutate(!!prune_var := replace(x = .data[[prune_var]],
                                      list = is.infinite(.data[[var]]),
                                      values = id))
  }

  check <- sum(!is.na(df[, prune_var]))
  if (check == nrow(df)) {
    msg_head <- cli::col_yellow("All the values are non-finite.")
    msg_body <- c("x" = "Verify the data, all values are non-finite.",
                  "i" = sprintf("Nb of non-finite values = %d", check))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_notfin_error")
  }

  df
}
