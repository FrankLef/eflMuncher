test_that("prune: Errors", {
  df <- data.frame(
    norm = rnorm(10),
    prune = NA_character_,
    prune_id = "id"
    )

  expect_error(prune(df = data.frame(), id = "id", func = identity, cols = "norm"),
               regexp = "argument .+ is missing")
  expect_error(prune(df, id = "id", func = NULL, cols = "norm"),
               class = "prune_error1")
  expect_error(prune(df, id = NA_character_, func = identity, cols = "norm"),
               class = "prune_error2")
  expect_error(prune(df, id = NA_character_, func = NULL),
               class = "prune_error3")


})

test_that("prune: reset", {
  df <- data.frame(
    unif = runif(5))

  # create new column if it does not exist
  out <- prune(df)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df
  target$prune_id <- NA_character_
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})
