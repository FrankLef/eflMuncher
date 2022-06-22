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

test_that("prune_oob_date: select the rows", {
  the_dates1 <- c(as.Date("1900-01-01"), as.Date("1900-01-02"), as.Date("1970-01-01"),
                  Sys.Date(), Sys.Date(), Sys.Date(),
                  as.Date("2029-12-31"), as.Date("2030-01-01"), as.Date("3000-01-01"))

  df_dates <- data.frame(
    id = seq_along(the_dates1),
    date1 = the_dates1
  )
  # cat("\n", "df_date", "\n")
  # print(df_dates)
  # cat("\n")

  out <-  prune_oob_date(df_dates, cols = "date1",
                        rng = as.Date(c("1970-01-01", "2029-12-31")))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c(T, T, F, F, F, F, F, T, T)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})
