# declarations ------------------------------------------------------------
id_old <- "old"  # id for old prune id
id_new <- "fence"  # current pruning identification

# tests -------------------------------------------------------------------

test_that("prune_fence_old: Input errors",
          {
            # skip("skip this test")

            eg <- data.frame(
              valX = c(-Inf, NA_real_, 1, 2, 3),
              valY = c(1, 2, 3, NA_real_, Inf),
              prune_id = factor(NA_character_)
            )

            expect_error(prune_fence_old(eg, x = "valX", y = "valY"),
                         class = "prune_fence_error1")
          })

test_that("prune_fence_old: all rows already pruned",
          {
            # skip("skip this test")

            eg <- data.frame(
              valX = 0:4,
              valY = 5:9,
              prune_id = factor(id_old)
            )

            expect_error(prune_fence_old(eg, x = "valX", y = "valY"),
                         class = "prune_fence_error2")
          })


test_that("prune_fence_slopes_old: errors",
          {
            # skip("skip this test")

            x <- c(0, 0, 0, Inf, 3)
            y <- c(0, 1, 2, 1, 4)
            expect_error(prune_fence_slopes_old(x, y), regexp = "Must be finite")

            x <- c(0, 0, 0, NA_real_, 3)
            y <- c(0, 1, 2, 1, 4)
            expect_error(prune_fence_slopes_old(x, y), regexp = "missing values")

            x <- c(0, 0)
            y <- c(0, 1)
            expect_error(prune_fence_slopes_old(x, y),
                         regexp = "Must have length >= 3")

            # Fencing doesn't work when all values are the same
            x <- rep(1, times = 5)
            y <- rep(2, times = 5)
            expect_error(prune_fence_slopes_old(x, y),
                         regexp = "All values.+are the same.+")

            # this will give same low and high slopes
            x <- c(0, rep(1, times = 5))
            y <- c(0, rep(2, times = 5))
            expect_error(prune_fence_slopes_old(x, y),
                         class = "prune_fence_calc_error1")
            expect_error(prune_fence_slopes_old(x, x),
                         class = "prune_fence_calc_error1")

          })

test_that("prune_fence_slopes_old",
          {
            # skip("skip this test")

            # test with simple non-negative data with a zero
            x_val <- c(0, 0, 1, 2, 3)
            y_val <- c(0, 1, 2, 1, 4)

            x_pos <- x_val[x_val > 0]
            y_pos <- y_val[y_val > 0]
            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- c("low_slope" = target_low,
                               "high_slope" = target_high)

            test_slopes <- prune_fence_slopes_old(x_val, y_val)

            expect_identical(test_slopes, target_slopes)


            # test with negative data with a zero
            x_val <- c(0, 0, 1, -2, 3)
            y_val <- c(0, -1, -2, 1, 4)

            x_pos <- x_val[x_val > 0]
            y_pos <- y_val[y_val > 0]
            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- c("low_slope" = target_low,
                               "high_slope" = target_high)

            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- c("low_slope" = target_low,
                               "high_slope" = target_high)

            test_slopes <- prune_fence_slopes_old(x_val, y_val)

            expect_identical(test_slopes, target_slopes)


            # test with data on a wide scale
            x_val <- c(0, 1e4, -1e4, 1e5, 1)
            y_val <- c(1e3, -1, 1e5, -1, -100)

            x_pos <- x_val[x_val > 0]
            y_pos <- y_val[y_val > 0]
            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- c("low_slope" = target_low,
                               "high_slope" = target_high)

            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- c("low_slope" = target_low,
                               "high_slope" = target_high)

            test_slopes <- prune_fence_slopes_old(x_val, y_val)

            expect_identical(test_slopes, target_slopes)


          })

test_that("prune_fence_df",
          {
            # skip("skip this test")

            eg <- data.frame(
              x = c(0, 1:5),
              y = c(2:6, 0),
              flag = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))

            x_pos <- eg$x[eg$x > 0]
            y_pos <- eg$y[eg$y > 0]
            low_slope <- min(x_pos) / max(y_pos)
            high_slope <- max(x_pos) / min(y_pos)
            the_slopes <- c("low_slope" = low_slope, "high_slope" = high_slope)

            target <- eg
            target$low <- target$x * low_slope
            target$high <- target$x * high_slope
            target$flag <- FALSE
            target$flag <- target$flag | target$y < target$low
            target$flag <- target$flag | target$y > target$high
            target <- target[, c("x", "y", "low", "high", "flag")]
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)

            test <- prune_fence_df(df = eg, x = "x", y = "y", specs = the_slopes)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(test)

            expect_identical(test, target)
          })

test_that("prune_fence_old",
          {
            # skip("skip this test")

            eg <- data.frame(
              x = c(0, 1:5),
              y = c(2:6, 0),
              prune_id = factor(c(NA_character_, NA_character_, id_old,
                                  NA_character_, id_old, NA_character_)))

            x_pos <- eg$x[eg$x > 0]
            y_pos <- eg$y[eg$y > 0]
            low_slope <- min(x_pos) / max(y_pos)
            high_slope <- max(x_pos) / min(y_pos)
            the_slopes <- c("low_slope" = low_slope, "high_slope" = high_slope)

            target <- eg
            target$prune_id <- factor(c(id_new, id_new, id_old,
                                        NA_character_, id_old, id_new),
                                      levels = c("old", "fence"))
            # change row.names to match test
            # NOTE: don't use row.names, does not do the job
            attr(target, which = "row.names") <- as.character(row.names(target))
            # cat("\n", crayon::bold$yellow("target"), "\n")
            # print(target)
            # print(attr(target, which = "row.names"))
            # print(levels(target$prune_id))
            #
            test <- prune_fence_old(df = eg, x = "x", y = "y",
                                prune_var = "prune_id", id = id_new,
                                details = FALSE)
            # cat("\n", crayon::bold$yellow("test"), "\n")
            # print(test)
            # print(attr(test, which = "row.names"))
            # print(levels(test$prune_id))

            expect_identical(test, target)
          })


test_that("prune_fence_old with custom data ",
          {

            # skip("skip this test")

            # NOTE: See plot below to visualize
            eg <- df <- data.frame(
              valX = c(0.0, 0.0, 0.3, 1.0, 3.0, 3.0, 3.0, 7.5, 8.0, 8.2),
              valY = c(0.0, 0.5, 6.0, 2.0, 0.0, 2.5, 8.1, 1.0, 7.0, 6.0),
              prune_id = factor(NA_character_)
            )

            target <- eg
            target$prune_id <- factor(c(NA_character_, id_new, id_new,
                                      NA_character_, id_new, NA_character_,
                                      NA_character_, NA_character_,
                                      NA_character_, NA_character_))
            attr(target, which = "row.names") <- as.character(row.names(target))

            test_df <- prune_fence_old(eg, x = "valX", y = "valY",
                                   details = FALSE)

            test_nb <- sum(!is.na(test_df$prune_id))

            expect_identical(test_df, target)
          })


# plot custom data --------------------------------------------------------

# if (FALSE) {
#
#   df <- data.frame(
#     valX = c(0.0, 0.0, 0.3, 1.0, 3.0, 3.0, 3.0, 7.5, 8.0, 8.2),
#     valY = c(0.0, 0.5, 6.0, 2.0, 0.0, 2.5, 8.1, 1.0, 7.0, 6.0),
#     prune_id = factor(NA_character_)
#   )
#
#   # the fence
#   lst <- prune_fence_old(df, x = "valX", y = "valY", details = TRUE)
#   lst
#
#   # replace NA for ploting
#   id_na <- "_NA"  # "NA, for ploting"
#   lst$df$prune_id <- forcats::fct_explicit_na(f = lst$df$prune_id,
#                                               na_level = id_na)
#
#   # note on intercept:
#   # Since the slope is not a difference (range), then, technically it is using
#   # the point (0,0) as a starting point.  Therefore the intercept used in
#   # the plot is the origin
#
#   # labels for the plot
#   labs <- ggplot2::labs(
#     title = sprintf("Fencing %d points", nrow(lst$df)),
#     subtitle = sprintf("low slope = %.2f, high slope = %.2f; %d oob",
#                        lst$specs["low_slope"], lst$specs["high_slope"],
#                        sum(lst$df$prune_id != id_na)),
#     x = NULL, y = NULL
#   )
#   # the plot
#   p <- ggplot2::ggplot(data = lst$df, ggplot2::aes(x = valX, y = valY)) +
#     ggplot2::geom_point(ggplot2::aes(color = prune_id, shape = prune_id), size = 5) +
#     ggplot2::geom_abline(slope = lst$specs["low_slope"], intercept = 0,
#                          linetype = "solid", color = "chocolate", size = 1) +
#     ggplot2::geom_abline(slope = lst$specs["high_slope"], intercept = 0,
#                          linetype = "solid", color = "chocolate", size = 1) +
#     ggplot2::scale_color_manual(values = c("_NA" = "darkgreen",
#                                            "fence" = "violetred")) +
#     ggplot2::scale_shape_manual(values = c("_NA" = 19,
#                                            "fence" = 18)) +
#     ggplot2::theme_light() +
#     ggplot2::theme(legend.position = "none") +
#     labs
#   p
#
#
# }


# plot iris ---------------------------------------------------------------


# if (FALSE) {
#
#   df <- data.frame(iris, prune_id = factor(NA_character_))
#
#   # the fence
#   lst <- prune_fence_old(df, x = "Sepal.Width", y = "Petal.Width",
#                      details = TRUE)
#   lst
#
#   # replace NA for ploting
#   id_na <- "_NA"  # "NA, for ploting"
#   lst$df$prune_id <- forcats::fct_explicit_na(f = lst$df$prune_id,
#                                               na_level = id_na)
#
#   # labels for the plot
#   labs <- ggplot2::labs(
#     title = sprintf("Fencing %d points", nrow(lst$df)),
#     subtitle = sprintf("low slope = %.2f, high slope = %.2f; %d oob",
#                        lst$specs["low_slope"], lst$specs["high_slope"],
#                        sum(lst$df$prune_id != id_na)),
#     x = NULL, y = NULL
#   )
#   # the plot
#   p <- ggplot2::ggplot(data = lst$df, ggplot2::aes(x = Sepal.Width,
#                                                    y = Petal.Width)) +
#     ggplot2::geom_point(ggplot2::aes(color = prune_id, shape = prune_id), size = 5) +
#     ggplot2::geom_abline(slope = lst$specs["low_slope"], intercept = 0,
#                          linetype = "solid", color = "chocolate", size = 1) +
#     ggplot2::geom_abline(slope = lst$specs["high_slope"], intercept = 0,
#                          linetype = "solid", color = "chocolate", size = 1) +
#     ggplot2::scale_color_manual(values = c("_NA" = "darkgreen",
#                                            "fence" = "violetred")) +
#     ggplot2::scale_shape_manual(values = c("_NA" = 19,
#                                            "fence" = 18)) +
#     ggplot2::theme_light() +
#     ggplot2::theme(legend.position = "none") +
#     labs
#   p
#
#
# }
