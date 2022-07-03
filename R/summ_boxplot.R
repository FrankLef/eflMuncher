#' Summary with \code{boxplot.stats}
#'
#' Summary with \code{boxplot.stats}.
#'
#' The summary allows to exclude pruned data from the stats.
#'
#' @param data Dataframe.
#' @param box_var Name of variable used by \code{boxplot.stats}.
#' @param group_var Name of grouping variable. If \code{NULL}, the summary will
#' only have one line for all rows, without grouping.
#' @param prune_var Name of variable with prune id. If \code{NULL}, no filter
#' is used to remove pruned rows.
#' @param is_log TRUE: Convert computations with \code{exp}.
#' @param coef Numeric constant to determine how far the whisker extends. If
#' zero, the whiskers will extend to extremes and no outlier will be returned.
#' See \code{grDevices::boxplot.stats} for more details.
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom stats setNames
#' @importFrom grDevices boxplot.stats
#'
#' @seealso grDevices boxplot.stats
#'
#' @source \url{https://stackoverflow.com/questions/56669653/boxplot-stats-in-dplyr-with-groups}
#'
#' @return Data.frame with boxplot.stats by \code{group_var}.
#' @export
summ_boxplot <- function(data, box_var = "ratio_log", group_var = NULL,
                         prune_var = NULL, is_log = TRUE, coef = 1.5) {
  checkmate::assertDataFrame(data, min.cols = 2, min.rows = 1)
  checkmate::assertNames(box_var, subset.of = names(data))
  checkmate::assertString(group_var, min.chars = 1, null.ok = TRUE)
  checkmate::assertString(prune_var, min.chars = 1, null.ok = TRUE)
  checkmate::assertFlag(is_log)
  checkmate::assertNumber(coef, lower = 0L)

  # SOURCE: very good source with more good details
  # https://stackoverflow.com/questions/56669653/boxplot-stats-in-dplyr-with-groups

  # remove the pruned rows if prune_id is not NULL
  if (!is.null(prune_var)) {
    checkmate::assertNames(prune_var, subset.of = names(data))
    data <- data |>
      dplyr::filter(is.na(.data[[prune_var]]))
  }

  # group by group if a grouping variable is provided
  if (!is.null(group_var)) {
    data <- data |>
      group_by(.data[[group_var]])
  }

  # get the boxplot stats
  the_names <- c('low_whisk','low_hinge','med','high_hinge','high_whisk')
  if (is_log) the_names <- paste(the_names, "log", sep = "_")

  # SOURCE: very good source with more good details
  # https://stackoverflow.com/questions/56669653/boxplot-stats-in-dplyr-with-groups
  out <- data |>
    dplyr::summarise(boxplot = list(stats::setNames(
      grDevices::boxplot.stats(.data[[box_var]], coef = coef)$stats,
      the_names))) |>
    tidyr::unnest_wider(.data$boxplot)

  # if log = TRUE then do the inverse with exp
  if (is_log) {
    out <- out |>
      mutate(
        low_whisk = exp(.data$low_whisk_log),
        low_hinge = exp(.data$low_hinge_log),
        med = exp(.data$med_log),
        high_hinge = exp(.data$high_hinge_log),
        high_whisk = exp(.data$high_whisk_log))
    }

  out
}
