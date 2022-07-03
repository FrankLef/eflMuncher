test_that("summ_boxplot: Input Errors", {
  df <- iris

  rgx <- "Must group by variables found in `.data`"
  expect_error(summ_boxplot(df, box_var = "Sepal.Length",
                            group_var = "wrong",
                            prune_var = NULL, log = TRUE),
               regexp = rgx)
})

test_that("summ_boxplot: prune_var = NULL, log = FALSE", {
  df <- iris

  out <- summ_boxplot(df, box_var = "Sepal.Length", group_var = "Species",
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

test_that("summ_boxplot: prune_var = NULL, log = TRUE", {
  df <- iris

  out <- summ_boxplot(df, box_var = "Sepal.Length", group_var = "Species",
                      prune_var = NULL, log = TRUE) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = round, digits = 1)) |>
    dplyr::select(Species, low_whisk, low_hinge, med, high_hinge, high_whisk) |>
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
    mutate(Species = as.factor(Species)) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = exp)) |>
    mutate(dplyr::across(.cols = where(is.numeric), .fns = round, digits = 1))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

test_that("summ_boxplot: group_var = NULL, prune_var = NULL, log = FALSE", {
  df <- iris

  out <- summ_boxplot(df, box_var = "Sepal.Length", log = FALSE) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  the_names <- c('low_whisk','low_hinge','med','high_hinge','high_whisk')
  target <- stats::setNames(grDevices::boxplot.stats(df$Sepal.Length)$stats,
                            nm = the_names)
  target <- data.frame(t(target))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})
