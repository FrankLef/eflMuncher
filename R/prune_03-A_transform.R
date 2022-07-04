prune_transform <- function(data, var, prune_var = "prune_id", func = log, ...) {
  checkmate::assertDataFrame(data)
  checkmate::assertNames(prune_var, subset.of = names(data))
  checkmate::assertNames(prune_var, subset.of = names(data))
  checkmate::assertFunction(func)

  data |>
    mutate(!!var := dplyr::if_else(is.na(.data[[prune_var]]),
                                   func(.data[[var]], ...),
                                   NA_real_))
}
