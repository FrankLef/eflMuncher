id_not <- "not"  # value not equal to default value of prune id
set.seed(123)
the_values <- c(NA_real_, NaN, -1, rlnorm(n = 7, meanlog = 10, sdlog = 3))
df <- data.frame(
  alpha = sample(the_values, size = length(the_values), replace = TRUE),
  num = the_values,
  prune_id = factor(x = c(rep(id_not, times = 3), rep(NA_character_, times = 7)))
  )
# cat("\n", "DF", "\n")
# print(df)

test_that("prune_trans ERROR: problem with data",
          code = {

            eg <- df
            eg$prune_id <- id_not

            # ERROR: no data available for transformation
            expect_error(prune_trans(eg, raw_var = "num",
                                     trans_var = "trans_num"),
                         class = "prune_trans_error")
          })

test_that("prune_trans - LOG",
          code = {
            # the bestNormalize transformation to use
            fnc_trans <- function(x) {
              bestNormalize::log_x(x, b = exp(1), standardize = FALSE)
            }

            # create the target
            target <- df
            # raw_data <- replace(target$num, target$prune_id != id_na, NA_real_)
            raw_data <- replace(target$num, !is.na(target$prune_id), NA_real_)
            trans <- fnc_trans(x = raw_data)
            target$trans_num <- trans$x.t

            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)
            # cat("\n", crayon::bold$yellow("trans"), "\n")
            # print(trans)

            eg <- df
            test <- prune_trans(eg, raw_var = "num", trans_var = "trans_num",
                                fnc_trans = fnc_trans, details = TRUE)
            # cat("\n", crayon::bold$yellow("test$data"), "\n")
            # print(test$data)
            # cat("\n", crayon::bold$yellow("test$trans"), "\n")
            # print(test$trans)

            expect_identical(test$data, target)
            expect_identical(test$trans, trans)
          })

test_that("prune_trans - BOXCOX",
          code = {
            # the bestNormalize transformation to use
            fnc_trans <- function(x) {
              bestNormalize::boxcox(x, standardize = FALSE)
            }

            # create the target
            target <- df
            # raw_data <- replace(target$num, target$prune_id != id_na, NA_real_)
            raw_data <- replace(target$num, !is.na(target$prune_id), NA_real_)
            trans <- fnc_trans(x = raw_data)
            target$trans_num <- trans$x.t

            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)
            # cat("\n", crayon::bold$yellow("trans"), "\n")
            # print(trans)

            eg <- df
            test <- prune_trans(eg, raw_var = "num", trans_var = "trans_num",
                                fnc_trans = fnc_trans, details = TRUE)
            # cat("\n", crayon::bold$yellow("test$data"), "\n")
            # print(test$data)
            # cat("\n", crayon::bold$yellow("test$trans"), "\n")
            # print(test$trans)

            expect_identical(test$data, target)
            expect_identical(test$trans, trans)
          })

# test_that("prune_trans - bestNormalize",
#           code = {
#             # the bestNormalize transformation to use
#             fnc_trans <- function(x) {
#               bestNormalize::bestNormalize(x, standardize = FALSE)
#             }
#
#             # create the target
#             target <- df
#             # raw_data <- replace(target$num, target$prune_id != id_na, NA_real_)
#             raw_data <- replace(target$num, !is.na(target$prune_id), NA_real_)
#             trans <- fnc_trans(x = raw_data)
#             target$trans_num <- trans$x.t
#
#             # cat("\n", crayon::bold$yellow("target"), "\n")
#             # print(target)
#             # cat("\n", crayon::bold$yellow("trans"), "\n")
#             # print(trans)
#
#             eg <- df
#             test <- prune_trans(eg, raw_var = "num", trans_var = "trans_num",
#                                 fnc_trans = fnc_trans, details = TRUE)
#             # cat("\n", crayon::bold$yellow("test$data"), "\n")
#             # print(test$data)
#             # cat("\n", crayon::bold$yellow("test$trans"), "\n")
#             # print(test$trans)
#             expect_identical(test$data, target)
#           })
