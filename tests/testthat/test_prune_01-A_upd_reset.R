id_not <- "not"  # value not equal to default value of prune id
id_new <- "new"  # current prunning identification

set.seed(123)
df <- data.frame(
  alpha = sample(letters, size = 9, replace = TRUE),
  num = round(runif(n = 9, min = -1, max = 1), 2),
  prune_id = NA_character_
  )
# flag num < -0.5
df <- df %>%
  mutate(prune_id = replace(x = prune_id, list = num < -0.5, values = id_not))
# df$prune_id[df$num < -0.5] <- id_not
# df$prune_id <- factor(df$prune_id)

# cat("\n", crayon::bold$yellow("df"), "\n")
# print(df)

test_that("prune_reset: tibble, create new column.",
          {
            target <- tibble::tibble(
              alpha = letters[1:3],
              prune_id = factor(NA_character_))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # str(target)
            # print(levels(target$prune_id))

            # create new column
            eg <- tibble::tibble(alpha = letters[1:3])
            test <- prune_reset(eg, prune_var = "prune_id")
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # str(test)
            # print(levels(test$prune_id))


            expect_identical(test, target)
          })

test_that("prune_reset: Create new column.",
          code = {

            # the target dataframe
            # target <- data.frame(alpha = letters[1:3], prune_id = factor(id_na))
            target <- data.frame(alpha = letters[1:3],
                                 prune_id = factor(NA_character_))

            # cat("\n", crayon::bold$yellow("target"), "\n")
            # str(target)

            # create new column
            eg <- data.frame(alpha = letters[1:3])
            test <- prune_reset(eg, prune_var = "prune_id")
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # str(test)
            expect_identical(test, target)

            # reset existing column
            eg <- data.frame(alpha = letters[1:3], prune_id = factor(id_not))
            eg <- data.frame(alpha = letters[1:3],
                             prune_id = NA_character_)
            test <- prune_reset(eg, prune_var = "prune_id")
            expect_identical(test, target)

          })

test_that("prune_reset: specific",
          code = {

            eg <- data.frame(
              alpha = 1:4,
              prune_id = factor(c(NA_character_, "a","b", "c")))

            # the target dataframe
            target <- eg %>%
              mutate(prune_id = replace(x = prune_id,
                                        list = (prune_id == "a"),
                                        values = NA_character_))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # str(target)

            # create new column
            test <- prune_reset(eg, prune_var = "prune_id", id = "a")
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # str(test)

            expect_identical(test, target)
          })


test_that("prune_upd",
          code = {

            target <- df %>%
              mutate(prune_id = replace(x = prune_id, list = num > 0.5,
                                        values = id_new),
                     prune_id = factor(prune_id, levels = c("not", "new")))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(str(target))

            eg <- df
            # cat("\n", crayon::bold$yellow("eg"), "\n")
            # print(eg)
            cond <- eg$num > 0.5
            test <- prune_upd(eg, cond, id = id_new)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(str(test))
            expect_identical(test, target)
          })

test_that("prune_reset then prune_upd",
          code = {
            # update after setting the entire prune_id to NA
            # i.e. with 0 levels since everything is NA_character_

            target <- df %>%
              mutate(prune_id = NA_character_) %>%
              mutate(prune_id = replace(x = prune_id, list = num > 0.5,
                                        values = id_new),
                     prune_id = as.factor(prune_id))
            # cat("\n", "target", "\n")
            # print(target)


            eg <- df %>%
              prune_reset()
            # cat("\n", crayon::bold$yellow("eg"), "\n")
            # print(eg)
            cond <- eg$num > 0.5
            test <- prune_upd(eg, cond, id = id_new)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(test)
            expect_identical(test, target)
          })


test_that("prune_upd ERROR: no row with default id",
          code = {

            eg <- df
            eg$prune_id <- factor(id_not)
            cond <- df$num < 0L

            # ERROR: There is no row with the default id ("ok")
            expect_error(prune_upd(eg, cond = cond, id = id_new),
                         class = "prune_upd_error2")

          })

test_that("prune_upd ERROR",
          code = {

            cond <- df$num < 0L
            cond[1] <- NA  # add NA to the first element

            # ERROR: some conditions are NA
            expect_error(prune_upd(df, cond = cond, id = id_new),
                         class = "prune_upd_error3")

          })
