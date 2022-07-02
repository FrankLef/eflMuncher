#' Summary with \code{n, sum}
#'
#' Summary with \code{n, sum}.
#'
#' The summary allows to exclude pruned data from the stats.
#'
#' @param data Dataframe.
#' @param group_var Name of grouping variable.
#' @param stats_var Name of variable used to compute the stats.
#' @param prune_var Name of variable with prune id. If \code{NULL}, no filter
#' is used to remove pruned rows.
#' @param log TRUE: Convert computations with \code{exp}.
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @return Data.frame with boxplot.stats by \code{group_var}.
#' @export
summ_stats <- function(data, group_var = "group", stats_var = "ratio_log",
                         prune_var = NULL, log = TRUE) {
  checkmate::assertDataFrame(data, min.cols = 2, min.rows = 1)
  checkmate::assertNames(c(group_var, stats_var), subset.of = names(data))
  checkmate::assertString(prune_var, min.chars = 1, null.ok = TRUE)

  # SOURCE: very good source with more good details
  # https://stackoverflow.com/questions/56669653/boxplot-stats-in-dplyr-with-groups

  # remove the pruned rows if prune_id is not NULL
  if (!is.null(prune_var)) {
    checkmate::assertNames(prune_var, subset.of = names(data))
    data <- data |>
      dplyr::filter(is.na(.data[[prune_var]]))
  }


  # depending on log, the name of the total column changes
  if (log) {
    # if log = TRUE then do the inverse with exp
    out <- data |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::summarise(nb = dplyr::n(),
                       tot_log = sum(.data[[stats_var]])) |>
      mutate(tot = exp(.data$tot_log))
  } else {
    out <- data |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::summarise(nb = dplyr::n(),
                       tot = sum(.data[[stats_var]]))
  }

  out
}
