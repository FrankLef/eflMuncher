#' Transform a Variable Using Pruned Data
#'
#' Transform a variable using pruned data.
#'
#' Transform a variable using pruned data. When the row has a prune id of
#' \code{NA}, the computation returns \code{NA}.
#'
#' @param data Dataframe.
#' @param var Name of variable with the result of the computation.
#' @param prune_var The name of the column with the prune id. Default is
#' "prune_id".
#' @param func Function used to transform \code{var}. Most common example
#' is \code{log}.
#' @param ... Additional arguments used by \code{func}.
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate if_else
#'
#' @return Data.frame.
#' @export
prune_transform <- function(data, var, prune_var = "prune_id", func = log, ...) {
  checkmate::assertDataFrame(data)
  checkmate::assertNames(c(var, prune_var), subset.of = names(data))
  checkmate::assertFunction(func)

  data |>
    mutate(!!var := dplyr::if_else(is.na(.data[[prune_var]]),
                                   func(.data[[var]], ...),
                                   NA_real_))
}
