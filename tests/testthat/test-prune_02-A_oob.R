set.seed(1234)
df_num <- data.frame(
  norm = round(rnorm(10), 1),
  unif = round(runif(10) - 0.1, 1),
  gamma = round(rgamma(10, shape = 1), 1),
  prune_id = c("oob_num", "oob_num", rep.int(NA_character_, 3),
               "id", "id", rep.int(NA_character_, 3))
  )
# df

test_that("prune_oob_num: select the rows", {
  df <- df_num
  # cat("\n", "df", "\n")
  # print(df)
  # cat("\n")

  out <-  prune_oob_num(df, cols = c("norm", "unif"), rng = c(0, Inf))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c(T, F, F, T, F, F, T, T, T, T)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})


test_that("prune_oob_num: used by prune_proc", {
  df <- df_num
  # cat("\n", "df", "\n")
  # print(df)
  # cat("\n")

  out <- prune_proc(df, id = "oob_num", func = prune_oob_num,
                    cols = c("norm", "unif"), rng = c(0, Inf))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df_num
  target$prune_id <- c("oob_num", NA_character_, NA_character_, "oob_num",
                       NA_character_, "id", "id", "oob_num", "oob_num",
                       "oob_num")
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})
