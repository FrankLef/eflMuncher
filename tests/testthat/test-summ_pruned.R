test_that("summ_pruned: Input errors", {
  df <- iris
  rgx <- "Assertion on \'prune_var\' failed"
  expect_error(summ_pruned(df), regexp = rgx)
  df$prune_id <- NA_character_
  rgx <- "Assertion on \'amt_var\' failed"
  expect_error(summ_pruned(df, amt_var = "wrong"), regexp = rgx)
})


test_that("summ_pruned: Without amt_var", {
  df <- iris
  df$prune_id <- NA_character_
  df$prune_id[c(1, 50, 100, 101, 150)] <- c("oob", "out", "maha", "oob", "out")

  out <- summ_pruned(df) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")


  target <- data.frame(
    prune_id = c("maha", "oob", "out", NA_character_),
    nb = c(1L, 2L, 2L, 145L),
    pct = c(round(100 * c(1, 2, 2) / nrow(df), 1), 96.7))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(sum(out$pct), 100)
  expect_identical(sum(out$nb), nrow(df))
  expect_identical(out, target)

})

test_that("summ_pruned: With amt_var", {
  df <- iris
  df$prune_id <- NA_character_
  df$prune_id[c(1, 50, 100, 101, 150)] <- c("oob", "out", "maha", "oob", "out")

  out <- summ_pruned(df, amt_var = "Sepal.Length") |>
    mutate(amt = round(amt, 1)) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")


  target <- data.frame(
    prune_id = c("maha", "oob", "out", NA_character_),
    nb = c(1L, 2L, 2L, 145L),
    pct = c(round(100 * c(1, 2, 2) / nrow(df), 1), 96.7),
    amt = c(5.7, 11.4, 10.9, 848.5),
    amt_pct = c(0.7, 1.3, 1.2, 96.8))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(sum(out$amt), sum(df$Sepal.Length))
  expect_identical(sum(out$amt_pct), 100)
  expect_identical(out, target)

})
