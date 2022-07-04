test_that("prune_transform: Input error", {
  data(iris_pruned)
  df <- iris_pruned
  # cat("\n", "df", "\n")
  # print(df)
  # cat("\n")

  rgx <- "Assertion on \'c[(]var, prune_var[)]\' failed"
  expect_error(prune_transform(df, new_var = "Petal.Length", var = "Petal.Length",
                               prune_var = "wrong", func = log),
               regexp = rgx)
  rgx <- "Assertion on \'c[(]var, prune_var[)]\' failed"
  expect_error(prune_transform(df, new_var = "Petal.Length", var = "wrong",
                               prune_var = "prune_id", func = log),
               regexp = rgx)
})

test_that("prune_transform: log", {
  data(iris_pruned)
  df <- iris_pruned |>
    dplyr::slice_head(n = 5)

  # cat("\n", "df", "\n")
  # str(df)
  # cat("\n")

  out <- df |>
    prune_transform(new_var = "Sepal.Length", var = "Sepal.Length",
                    prune_var = "prune_id", func = log)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df |>
    mutate(Sepal.Length = dplyr::if_else(is.na(prune_id),
                                         log(Sepal.Length),
                                         NA_real_))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

