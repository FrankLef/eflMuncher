
# declarations ------------------------------------------------------------

id_old <- "old"  # value not equal to default value of prune id
id_new <- "rng"  # current pruning identification


# tests -------------------------------------------------------------------


test_that("prune_range ERROR: invalid range min > max",
          {
            # ERROR: The prune range is invalid
            rng <- c(1, -1)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = 1:9,
              prune_id = factor(NA_character_)
            )

            expect_error(prune_oob(df = eg, var = "beta", rng = rng),
                         regexp = ".+Must be sorted.+")
          })

test_that("prune_range ERROR: range not wide enough",
          {
            rng <- c(1, 1 + .Machine$double.eps)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = 1:9,
              prune_id = factor(NA_character_)
            )

            # ERROR: The prune range is invalid
            rng <- c(1, 1 + .Machine$double.eps^0.51)

            expect_error(prune_oob(df = eg, var = "beta", rng = rng),
                         class = "prune_range_error1")
          })

test_that("prune_range ERROR: Non-finite values",
          {
            # skip("skip this")
            # range used to prune data
            rng <- c(1L, Inf)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = c(1:4, NA_real_, 6:9),
              prune_id = factor(NA_character_)
            )

            expect_error(prune_oob(df = eg, var = "beta", rng = rng),
                         class = "prune_range_error2")

          })

test_that("prune_fence: all rows already pruned",
          {
            # skip("skip this test")

            rng <- c(1L, Inf)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = 1:9,
              prune_id = factor(id_old)
            )

            expect_error(prune_oob(df = eg, var = "beta", rng = rng),
                         class = "prune_range_error3")
          })

test_that("prune_range",
          {
            # skip("skip this")
            # range used to prune data
            rng <- c(2L, 8L)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = 1:9,
              prune_id = factor(NA_character_)
            )
            # cat("\n", crayon::bold$yellow("eg"), "\n")
            # print(eg)

            target <- eg
            target$prune_id <- factor(c(
              id_new, NA_character_, NA_character_,
              NA_character_, NA_character_, NA_character_,
              NA_character_, NA_character_, id_new))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)

            test <- prune_oob(df = eg, var = "beta", rng = rng, id = id_new)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(test)

            expect_equal(test, target, ignore_attr = TRUE)

          })

test_that("prune_range with preexisting id",
          {
            # skip("skip this")
            # range used to prune data
            rng <- c(2L, 8L)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = 1:9,
              prune_id = factor(c(
                id_new, id_new, NA_character_,
                NA_character_, id_old, NA_character_,
                NA_character_, id_new, id_new))
            )
            # cat("\n", crayon::bold$yellow("eg"), "\n")
            # print(eg)

            target <- eg
            target$prune_id <- factor(c(
              id_new, NA_character_, NA_character_,
              NA_character_, id_old, NA_character_,
              NA_character_, NA_character_, id_new))
            # change row.names to match test
            # NOTE: don't use row.names, does not do the job
            attr(target, which = "row.names") <- as.character(row.names(target))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)

            test <- prune_oob(df = eg, var = "beta", rng = rng, id = id_new)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(test)

            expect_identical(test, target)

          })

test_that("prune_range with many preexisting SAME id",
          {
            # skip("skip this")
            # range used to prune data
            rng <- c(2L, 8L)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = 1:9,
              prune_id = factor(c(
                id_new, id_new, id_old,
                NA_character_, id_new, NA_character_,
                NA_character_, id_new, id_new))
            )
            # cat("\n", crayon::bold$yellow("eg"), "\n")
            # print(eg)

            target <- eg
            target$prune_id <- factor(c(
              id_new, NA_character_, id_old,
              NA_character_, NA_character_, NA_character_,
              NA_character_, NA_character_, id_new))
            # change row.names to match test
            # NOTE: don't use row.names, does not do the job
            attr(target, which = "row.names") <- as.character(row.names(target))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)

            test <- prune_oob(df = eg, var = "beta", rng = rng, id = id_new)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(test)

            expect_identical(test, target)

          })

test_that("prune_range with many preexisting SAME id causing WARNING",
          {
            # skip("skip this")
            # range used to prune data
            rng <- c(2L, 8L)
            eg <- data.frame(
              alpha = letters[1:9],
              beta = 1:9,
              prune_id = factor(c(
                id_new, id_new, NA_character_,
                NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_))
            )
            # cat("\n", crayon::bold$yellow("eg"), "\n")
            # print(eg)

            target <- eg
            target$prune_id <- factor(c(
              id_new, id_new, NA_character_,
              NA_character_, NA_character_, NA_character_,
              NA_character_, NA_character_, id_old),
              levels = c(id_new, id_old))
            # change row.names to match test
            # NOTE: don't use row.names, does not do the job
            attr(target, which = "row.names") <- as.character(row.names(target))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)
            # print(levels(target$prune_id))

            test <- prune_oob(df = eg, var = "beta", rng = rng, id = id_old)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(test)
            # print(levels(test$prune_id))

            expect_identical(test, target)

          })
