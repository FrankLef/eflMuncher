test_that("summ_boxplot: Input Errors", {
  df <- iris

  rgx <- "Assertion on \'c[(]group_var, box_var[)]\'"
  expect_error(summ_boxplot(df, group_var = "wrong", box_var = "Sepal.Length",
                            prune_var = NULL, log = TRUE),
               regexp = rgx)
})

test_that("summ_boxplot: log = FALSE", {
  df <- iris

  out <- summ_boxplot(df, group_var = "Species", box_var = "Sepal.Length",
                      prune_var = NULL, log = FALSE) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    Species = c("setosa", "versicolor", "virginica"),
    low_whisk = c(4.3, 4.9, 5.6),
    low_hinge = c(4.8, 5.6, 6.2),
    med = c(5, 5.9, 6.5),
    high_hinge = c(5.2, 6.3, 6.9),
    high_whisk = c(5.8, 7, 7.9)) |>
    mutate(Species = as.factor(Species))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

test_that("summ_boxplot: log = TRUE", {
  df <- iris

  out <- summ_boxplot(df, group_var = "Species", box_var = "Sepal.Length",
                      prune_var = NULL, log = TRUE) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = round, digits = 1)) |>
    dplyr::select(Species, low_whisk, low_hinge, med, high_hinge, high_whisk) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    Species = c("setosa", "versicolor", "virginica"),
    low_whisk = c(73.7, 134.3, 270.4),
    low_hinge = c(121.5, 270.4, 492.7),
    med = c(148.4, 365.0, 665.1),
    high_hinge = c(181.3, 544.6, 992.3),
    high_whisk = c(330.3, 1096.6, 2697.3)) |>
    mutate(Species = as.factor(Species))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

