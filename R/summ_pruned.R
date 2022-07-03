#' Summary of Pruned Rows
#'
#' Summary of pruned rows.
#'
#' Summary of pruned rows with info on the number of rows and the sum of
#' an optional variable identified by \code{amt_var},
#'
#' @param data Data.frame.
#' @param prune_var Name of variable with prune id. Default value is "prune_id".
#' @param amt_var Name of optional variable to sum. If NULL (default) then no
#' sum of amount will appear in the summary.
#'
#' @importFrom rlang .data
#' @importFrom dplyr group_by ungroup summarize mutate relocate
#'
#' @return Dataframe with summary info.
#' @export
summ_pruned <- function(data, prune_var = "prune_id", amt_var = NULL) {
  checkmate::assertDataFrame(data, min.cols = 2, min.rows = 1)
  checkmate::assertNames(prune_var, subset.of = names(data))

  if (!is.null(amt_var)) {
    checkmate::assertNames(amt_var, subset.of = names(data))
    out <- data |>
      dplyr::group_by(.data[[prune_var]]) |>
      dplyr::summarize(
        nb = dplyr::n(),
        amt = sum(.data[[amt_var]])
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(pct = round(100 * .data$nb / sum(.data$nb), 1),
             amt_pct = round(100 * .data$amt / sum(.data$amt), 1)) |>
      dplyr::relocate(.data$pct, .after = .data$nb)
  } else {
    out <- data |>
      dplyr::group_by(.data[[prune_var]]) |>
      dplyr::count(name = "nb") |>
      dplyr::ungroup() |>
      dplyr::mutate(pct = round(100 * .data$nb / sum(.data$nb), 1))
  }
  out
}
