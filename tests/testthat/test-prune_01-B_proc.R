test_that("prune_proc: errors", {

  expect_error(prune_proc(data = data.frame()),
               regexp = "Assertion on \'data\' failed")

  df <- data.frame(
    unif = runif(5),
    prune_var = "id")

  expect_error(prune_proc(data = df, id = NA_character_),
               regexp = "Assertion on \'id\' failed")

  expect_error(prune_proc(data = df, id = "id", prune_var = "wrong",
                          func = identity, cols = "unif"),
               regexp = "Assertion on \'prune_var\' failed")

  expect_error(prune_proc(data = df, id = "id", prune_var = "prune_var",
                          func = "", cols = "unif"),
               regexp = "Assertion on \'func\' failed")
  expect_error(prune_proc(data = df, id = "id", prune_var = "prune_var",
                          func = identity, cols = "wrong"),
               regexp = "Assertion on \'cols\' failed")
  # id may not be in cols
  expect_error(prune_proc(data = df, id = "unif", prune_var = "prune_var",
                          func = identity, cols = "unif"),
               regexp = "Assertion on \'id\' failed")
})
