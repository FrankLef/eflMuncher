#' Transform a Variable Using Pruned Data
#'
#' Transform a variable using pruned data.
#'
#' Transform a variable using pruned data. When the row has a prune id of
#' \code{NA}, the computation returns \code{NA}.
#'
#' @param data Dataframe.
#' @param new_var Name of new variable with the result of the computation.
#' @param var Name of variable to transform.
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
prune_transform <- function(data, new_var, var, prune_var = "prune_id", func = log, ...) {
  checkmate::assertDataFrame(data)
  checkmate::assertNames(new_var, type = "strict")
  checkmate::assertNames(c(var, prune_var), subset.of = names(data))
  checkmate::assertFunction(func)

  data |>
    mutate(!!new_var := dplyr::if_else(is.na(.data[[prune_var]]),
                                   func(.data[[var]], ...),
                                   NA_real_))
}
