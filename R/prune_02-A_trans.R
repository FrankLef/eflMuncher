#' Transform pruned data with \code{bestNormalize}
#'
#' Transform pruned data with \code{bestNormalize}.
#'
#' Transform pruned data where the \code{prune_var == NA} using the
#' \code{bestNormalize} package.
#'
#' @param df dataframe
#' @param raw_var String, name of column to process
#' @param trans_var String, name of the column with transformed data
#' @param prune_var String, name of the column with the prune identifications
#' @param fnc_trans Function, created with the \code{bestNormalize}
#'  package. If NULL, the default function \code{bestNormalize::no_transform}
#'  is used.
#' @param details TRUE = create a list with the data and \code{bestNormalize}
#'  object.
#'
#' @return dataframe with transformed values
#' @export
#'
#' @examples
#' # the example dataframe
#' df <- data.frame(
#'  num = rlnorm(n = 5, meanlog = 1, sdlog = 2),
#'  prune_id = factor(NA_character_))
#' # Create the transformation function using the `bestNormalize` package
#' fnc_trans <- function(x) {
#'  bestNormalize::log_x(x, b = exp(1), standardize = FALSE)
#' }
#' # transform the data
#' df <- prune_trans(df, raw_var = "num", trans_var = "num_log",
#'  fnc_trans = fnc_trans, details = TRUE)
prune_trans <- function(df, raw_var, trans_var, prune_var = "prune_id",
                        fnc_trans = NULL, details = FALSE) {
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)
  checkmate::assertNames(names(df), must.include = c(raw_var, prune_var))
  checkmate::assertString(trans_var, min.chars = 1)
  checkmate::assertFunction(fnc_trans, null.ok = TRUE)
  checkmate::assertFlag(details)

  # set pruned values to NA to avoid using them
  raw_data <- df %>%
    dplyr::mutate(!!raw_var := replace(.data[[raw_var]],
                                list = !is.na(.data[[prune_var]]),
                                values = NA_real_)) %>%
    dplyr::pull(.data[[raw_var]])


  # rows with default prune id must be available
  check <- all(is.na(raw_data))
  if(check) {
    msg_head <- cli::col_yellow("The vector of raw values is empty:")
    msg_body <- c("i" = "Verify if all prune flags = TRUE",
                  "i" = "Verify if some values are finite")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_trans_error"
    )
  }

  if (!is.null(fnc_trans)) {
    # change the transform object with the new data
    trans <- fnc_trans(x = raw_data)
  } else {
    trans <- bestNormalize::no_transform(x = raw_data)

    msg_head <- cli::col_yellow("bestNormalize::no_transform will be used.")
    msg_body <- c("x" = "Are jou sure? Why would you do that?.")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::warn(
      message = msg,
      class = "prune_trans_warning"
    )
  }

  # transform the data
  df[, trans_var] <- trans$x.t


  if (!details) {
    out <- df
  } else {
    out <- list("data" = df, "trans" = trans)
  }

  out
}
