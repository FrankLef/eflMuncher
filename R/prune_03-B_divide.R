#' Divide 2 Variables Using Pruned Data
#'
#' Divide 2 variables using pruned data.
#'
#' Divide 2 variables using pruned data. When the row has a prune id of
#' \code{NA}, the computation returns \code{NA}.
#'
#' @param data Dataframe.
#' @param new_var Name of new variable with the result of the computation.
#' @param num_var Name of numerator variable.
#' @param den_var Name of denominator variable.
#' @param prune_var The name of the column with the prune id. Default is
#' "prune_id".
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate if_else
#'
#' @return Data.frame.
#' @export
prune_divide <- function(data, new_var, num_var, den_var, prune_var = "prune_id") {
  checkmate::assertDataFrame(data)
  checkmate::assertNames(new_var, type = "strict")
  checkmate::assertNames(c(num_var, den_var, prune_var), subset.of = names(data))

  data |>
    mutate(!!new_var := dplyr::if_else(is.na(.data[[prune_var]]),
                                       .data[[num_var]] / .data[[den_var]],
                                       NA_real_))
}
