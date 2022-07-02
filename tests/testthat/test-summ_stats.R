test_that("summ_stats: Input Errors", {
  df <- iris

  rgx <- "Assertion on \'c[(]group_var, stats_var[)]\'"
  expect_error(summ_stats(df, group_var = "wrong", stats_var = "Sepal.Length",
                            prune_var = NULL, log = TRUE),
               regexp = rgx)
})

test_that("summ_stats: log = FALSE", {
  df <- iris

  out <- summ_stats(df, group_var = "Species", stats_var = "Sepal.Length",
                      prune_var = NULL, log = FALSE) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    Species = c("setosa", "versicolor", "virginica"),
    nb = 50L,
    tot = c(250.3, 296.8, 329.4)) |>
    mutate(Species = as.factor(Species))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

test_that("summ_stats: log = TRUE", {
  df <- iris

  out <- summ_stats(df, group_var = "Species", stats_var = "Sepal.Length",
                      prune_var = NULL, log = TRUE) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = round, digits = 1)) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  the_num <- c(250.3, 296.8, 329.4)
  target <- data.frame(
    Species = c("setosa", "versicolor", "virginica"),
    nb = 50,
    tot_log = the_num,
    tot = round(exp(the_num), 1)) |>
    mutate(Species = as.factor(Species))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

