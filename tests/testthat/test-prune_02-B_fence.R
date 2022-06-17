# declarations ------------------------------------------------------------
id_old <- "old"  # id for old prune id
id_new <- "fence"  # current pruning identification

# tests -------------------------------------------------------------------

test_that("prune_fence: Input errors",
          {

            expect_error(prune_fence(data.frame(), cols = c("X", "Y")),
                         regexp = "Assertion on \'data\' failed")


            df <- data.frame(
              X = c(-Inf, NA_real_, 1, 2, 3),
              Y = c(1, 2, 3, NA_real_, Inf),
              prune_id = NA_character_
            )

            expect_error(prune_fence(df, cols = c("valX", "valY")),
                         regexp = "Assertion on \'cols\' failed")

            expect_error(prune_fence(df, cols = c("X", "Y")),
                         regexp = "Contains missing values")

            df <- data.frame(
              X = 1:5,
              Y = c(1, 2, 3, NA_real_, 5),
              prune_id = NA_character_
              )
            expect_error(prune_fence(df, cols = c("X", "Y")),
                         regexp = "Contains missing values")

            df <- data.frame(
              X = 1:5,
              Y = c(1, 2, 3, Inf, 5),
              prune_id = NA_character_
            )
            expect_error(prune_fence(df, cols = c("X", "Y")),
                         regexp = "Must be finite")

          })

test_that("prune_fence: Offset errors",
          {
            # skip("skip this test")

            df <- data.frame(
              X = -1:3,
              Y = 5:9,
              prune_id = NA_character_
            )

            expect_error(prune_fence(df, cols = c("X", "Y"), is_offset = FALSE),
                         regexp = "Assertion on \'x\' failed")

            df <- data.frame(
              X = 0:4,
              Y = -1:3,
              prune_id = NA_character_
            )

            expect_error(prune_fence(df, cols = c("X", "Y"), is_offset = FALSE),
                         regexp = "Assertion on \'y\' failed")

          })


test_that("prune_fence_slopes: errors",
          {
            # skip("skip this test")

            x <- c(0, 0, 0, Inf, 3)
            y <- c(0, 1, 2, 1, 4)
            expect_error(prune_fence_slopes(x, y), regexp = "Must be finite")

            x <- c(0, 0, 0, NA_real_, 3)
            y <- c(0, 1, 2, 1, 4)
            expect_error(prune_fence_slopes(x, y), regexp = "missing values")

            x <- c(0, 0)
            y <- c(0, 1)
            expect_error(prune_fence_slopes(x, y),
                         regexp = "Must have length >= 3")

            # when all <- 0 x_pos or y_pos will be empty
            x <- c(0, 0, 0)
            y <- 1:3
            expect_error(prune_fence_slopes(x, y),
                         regexp = "length[(]x_pos[)] not greater than 0")
            x <- 1:3
            y <- c(0, 0, 0)
            expect_error(prune_fence_slopes(x, y),
                         regexp = "length[(]y_pos[)] not greater than 0")


            # Fencing doesn't work when all values are the same
            x <- rep(1, times = 5)
            y <- rep(2, times = 5)
            expect_error(prune_fence_slopes(x, y),
                         class = "prune_fence_slopes_error1")

            # this will give constant values since only positive
            # values are used to compute the slopes
            x <- c(0, rep(1, times = 5))
            y <- c(0, rep(2, times = 5))
            expect_error(prune_fence_slopes(x, y),
                         class = "prune_fence_slopes_error1")
            expect_error(prune_fence_slopes(x, x),
                         class = "prune_fence_slopes_error1")

          })

test_that("prune_fence_slopes",
          {
            # skip("skip this test")

            # test with simple non-negative data with a zero
            x_val <- c(0, 0, 1, 2, 3)
            y_val <- c(0, 1, 2, 1, 4)

            x_pos <- x_val[x_val > 0]
            y_pos <- y_val[y_val > 0]
            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- list("low" = target_low, "high" = target_high)

            test_slopes <- prune_fence_slopes(x_val, y_val)

            expect_identical(test_slopes, target_slopes)


            # test with negative data with a zero
            x_val <- c(0, 0, 1, -2, 3)
            y_val <- c(0, -1, -2, 1, 4)

            x_pos <- x_val[x_val > 0]
            y_pos <- y_val[y_val > 0]
            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- list("low" = target_low, "high" = target_high)


            test_slopes <- prune_fence_slopes(x_val, y_val)

            expect_identical(test_slopes, target_slopes)


            # test with data on a wide scale
            x_val <- c(0, 1e4, -1e4, 1e5, 1)
            y_val <- c(1e3, -1, 1e5, -1, -100)

            x_pos <- x_val[x_val > 0]
            y_pos <- y_val[y_val > 0]
            target_low <- min(x_pos) / max(y_pos)
            target_high <- max(x_pos) / min(y_pos)
            target_slopes <- list("low" = target_low, "high" = target_high)


            test_slopes <- prune_fence_slopes(x_val, y_val)

            expect_identical(test_slopes, target_slopes)


          })
