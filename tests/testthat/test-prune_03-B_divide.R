test_that("prune_divide: Input error", {
  data(iris_pruned)
  df <- iris_pruned
  # cat("\n", "df", "\n")
  # print(df)
  # cat("\n")

  rgx <- "Assertion on \'c[(]num_var, den_var, prune_var[)]\' failed"
  expect_error(prune_divide(df,
                               new_var = "Petal.Ratio",
                               num_var = "Petal.Length",
                               den_var = "Petal.Width",
                               prune_var = "wrong"),
               regexp = rgx)
  rgx <- "Assertion on \'c[(]num_var, den_var, prune_var[)]\' failed"
  expect_error(prune_divide(df,
                               new_var = "Petal.Ratio",
                               num_var = "wrong",
                               den_var = "Petal.Width",
                               prune_var = "prune_id"),
               regexp = rgx)
})

test_that("prune_divide", {
  data(iris_pruned)
  df <- iris_pruned |>
    dplyr::slice_head(n = 5)

  # cat("\n", "df", "\n")
  # str(df)
  # cat("\n")

  out <- df |>
    prune_divide(new_var = "Petal.Ratio",
                    num_var = "Petal.Length",
                    den_var = "Petal.Width",
                    prune_var = "prune_id")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df |>
    mutate(Petal.Ratio = dplyr::if_else(is.na(prune_id),
                                         Petal.Length / Petal.Width,
                                         NA_real_))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

