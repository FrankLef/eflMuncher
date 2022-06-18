
# prune_fence errors ------------------------------------------------------

test_that("prune_fence: Input errors", {

  expect_error(prune_fence(data.frame(), cols = c("X", "Y")),
               regexp = "Assertion on \'data\' failed")


  df <- data.frame(
    X = c(-Inf, NA_real_, 1, 2, 3),
    Y = c(1, 2, 3, NA_real_, Inf),
    prune_id = NA_character_
  )

  # names don't exist in data.frame
  expect_error(prune_fence(df, cols = c("valX", "valY")),
               regexp = "Assertion on \'cols\' failed")

  # values are not finite
  expect_error(prune_fence(df, cols = c("X", "Y")),
               regexp = "Must be finite")


  df <- data.frame(
    X = 1:5,
    Y = c(1, 2, 3, Inf, 5),
    prune_id = NA_character_
  )
  expect_error(prune_fence(df, cols = c("X", "Y")),
               regexp = "Must be finite")
  })

test_that("prune_fence: Offset errors", {
  # skip("skip this test")

  df <- data.frame(
    X = -1:3,
    Y = 5:9,
    prune_id = NA_character_
  )

  expect_error(prune_fence(df, cols = c("X", "Y"), is_offset = FALSE),
               regexp = "Assertion on \'x_scaled\' failed")

  df <- data.frame(
    X = 0:4,
    Y = -1:3,
    prune_id = NA_character_
  )

  expect_error(prune_fence(df, cols = c("X", "Y"), is_offset = FALSE),
               regexp = "Assertion on \'y_scaled\' failed")
  })



# prune_fence_slopes ------------------------------------------------------


test_that("prune_fence_slopes: Errors", {
  # skip("skip this test")

  x <- c(0, 0, 0, Inf, 3)
  y <- c(0, 1, 2, 1, 4)
  expect_error(prune_fence_slopes(x, y), regexp = "Must be finite")

  x <- c(0, 0)
  y <- c(0, 1)
  expect_error(prune_fence_slopes(x, y),
               regexp = "Must have length >= 3")

  # when all <- 0 x_pos or y_pos will be empty
  x <- c(0, 0, 0)
  y <- 1:3
  rgx <- "There are only [[:digit:]] positive values in scaled x"
  expect_error(prune_fence_slopes(x, y), regexp = rgx)
  x <- 1:3
  y <- c(0, 0, 0)
  rgx <- "There are only [[:digit:]] positive values in scaled y"
  expect_error(prune_fence_slopes(x, y), regexp = rgx)


  # Fencing doesn't work when all values are the same
  # that is, when at least one of the ranges are very small
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

test_that("prune_fence_slopes", {
  # skip("skip this test")

  # test with simple non-negative data with a zero
  x_val <- c(0, 0, 1, 2, 3)
  y_val <- c(0, 1, 2, 1, 4)

  x_pos <- x_val[x_val > 0]
  y_pos <- y_val[y_val > 0]
  target_small <- min(x_pos) / max(y_pos)
  target_big <- max(x_pos) / min(y_pos)
  target_slopes <- list("small" = target_small, "big" = target_big)

  test_slopes <- prune_fence_slopes(x_val, y_val)

  expect_identical(test_slopes, target_slopes)


  # test with negative data with a zero
  x_val <- c(0, 0, 1, -2, 3)
  y_val <- c(0, -1, -2, 1, 4)

  x_pos <- x_val[x_val > 0]
  y_pos <- y_val[y_val > 0]
  target_small <- min(x_pos) / max(y_pos)
  target_big <- max(x_pos) / min(y_pos)
  target_slopes <- list("small" = target_small, "big" = target_big)


  test_slopes <- prune_fence_slopes(x_val, y_val)

  expect_identical(test_slopes, target_slopes)

  # test when the range of x and range of y are the same
  x <- c(1 , 2.1, 3.2, 4.3, 5)
  y <- c(1 , 1.2, 2.3, 3.4, 5)

  x_pos <- x_val[x_val > 0]
  y_pos <- y_val[y_val > 0]
  target_small <- min(x_pos) / max(y_pos)
  target_big <- max(x_pos) / min(y_pos)
  target_slopes <- list("small" = target_small, "big" = target_big)


  test_slopes <- prune_fence_slopes(x_val, y_val)

  expect_identical(test_slopes, target_slopes)


  # test with data on a wide scale
  x_val <- c(0, 1e4, -1e4, 1e5, 1)
  y_val <- c(1e3, -1, 1e5, -1, -100)

  x_pos <- x_val[x_val > 0]
  y_pos <- y_val[y_val > 0]
  target_small <- min(x_pos) / max(y_pos)
  target_big <- max(x_pos) / min(y_pos)
  target_slopes <- list("small" = target_small, "big" = target_big)


  test_slopes <- prune_fence_slopes(x_val, y_val)

  expect_identical(test_slopes, target_slopes)
  })


# prune_fence -------------------------------------------------------------

test_that("prune_fence: Easy example", {

  df <- data.frame(
    x = c(0, 1:5, NA_real_),
    y = c(NA_real_, 2:6, 0),
    prune_id = NA_character_)

  x_pos <- df$x[df$x > 0]
  y_pos <- df$y[df$y > 0]
  slopes <- list("small" = min(x_pos) / max(y_pos),
                 "big" = max(x_pos) / min(y_pos))
  fences <- list(
    "small" = df$x * slopes$small,
    "big" = df$x * slopes$big
  )

  target <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  out <- prune_fence(df, cols = c("x", "y"), is_offset = TRUE)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  expect_identical(out, target)
  })

test_that("prune_fence: Full example", {
  data(fences_df)
  df <- fences_df

  out <- prune_fence(df, cols = c("X", "Y"), info = TRUE)
  # cat("\n", "out", "\n")
  # print(out$results)
  # cat("\n")

  target <- df
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out$oob, target$oob)
  })

test_that("prune_fence: Full example with NA", {
  data(fencesNA_df)
  df <- fencesNA_df

  out <- prune_fence(df, cols = c("X", "Y"), info = TRUE)
  # cat("\n", "out", "\n")
  # print(out$results)
  # cat("\n")

  target <- df
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out$oob, target$oob)
})

test_that("prune_fence: Iris", {
  data(iris)
  df <- iris

  out <- prune_fence(df, cols = c("Sepal.Width", "Petal.Width"))
  out <- out
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  expect_identical(sum(out), 28L)
})


# plot fence example ------------------------------------------------------


# plot example
# data(fences_df)
# df <- fences_df
# out <- prune_fence(df, cols = c("X", "Y"), is_offset = TRUE, info = TRUE)
# df_plot <- out$results
# df_plot$label <- paste0("(", df_plot$x, ", ", df_plot$y, ")")

# the plot
# the_ylim <- c(NA_real_, ceiling(max(df_plot$y)))
# the_subtitle <- sprintf("%d inside the fence, %d outside the fence.",
#                         sum(!df_plot$oob), sum(df_plot$oob))
# the_labs <- ggplot2::labs(
#   title = "Example of data with a fence",
#   subtitle = the_subtitle,
#   x = "X", y = "Y"
# )
# p <- ggplot2::ggplot(data = df_plot, ggplot2::aes(x = x, y = y)) +
#   ggplot2::geom_point(ggplot2::aes(color = oob, shape = oob, size = oob)) +
#   ggplot2::geom_line(ggplot2::aes(y = small),
#                      linetype = "dotdash", color = "chocolate", size = 1) +
#   ggplot2::geom_line(ggplot2::aes(y = big),
#                      linetype = "dotdash", color = "chocolate", size = 1) +
#   ggplot2::geom_text(ggplot2::aes(label = label), nudge_y = -0.25,
#                      color = "navyblue") +
#   ggplot2::scale_color_manual(values = c("FALSE" = "darkgreen",
#                                          "TRUE" = "violetred")) +
#   ggplot2::scale_shape_manual(values = c("FALSE" = 19,
#                                          "TRUE" = 18)) +
#   ggplot2::scale_size_manual(values = c("TRUE" = 5, "FALSE" = 4)) +
#   ggplot2::coord_cartesian(ylim = the_ylim) +
#   ggplot2::theme_light() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::theme(title = ggplot2::element_text(color = "midnightblue")) +
#   the_labs
# p



# plot fenceNA example ----------------------------------------------------

# plot example
# data(fencesNA_df)
# df <- fencesNA_df
# out <- prune_fence(df, cols = c("X", "Y"), is_offset = TRUE, info = TRUE)
# df_plot <- out$results
# df_plot
# write labels before replacing NA
# df_plot$label <- paste0("(", df_plot$x, ", ", df_plot$y, ")")
# replace NA with 0
# df_plot$x[is.na(df_plot$x)] <- 0
# df_plot$y[is.na(df_plot$y)] <- 0
# df_plot
# create df for fences without the NA
# df_fences <- df_plot
# df_fences <- df_fences[!is.na(df_fences$small) & !is.na(df_fences$big), ]

# the plot
# the_xlim <- c(NA_real_, ceiling(max(df_plot$x)))
# the_ylim <- c(NA_real_, ceiling(max(df_plot$y)))
# the_subtitle <- sprintf("%d inside the fence, %d outside the fence.",
#                         sum(!df_plot$oob), sum(df_plot$oob))
# the_labs <- ggplot2::labs(
#   title = "Example of data with a fence with NA",
#   subtitle = the_subtitle,
#   x = "X", y = "Y"
# )
# p <- ggplot2::ggplot(data = df_plot, ggplot2::aes(x = x, y = y)) +
#   ggplot2::geom_point(ggplot2::aes(color = oob, shape = oob, size = oob)) +
#   ggplot2::geom_line(df_fences, mapping = ggplot2::aes(x = x, y = small),
#                      inherit.aes = FALSE,
#                      linetype = "dotdash", color = "chocolate", size = 1) +
#   ggplot2::geom_line(df_fences, mapping = ggplot2::aes(x = x, y = big),
#                      inherit.aes = FALSE,
#                      linetype = "dotdash", color = "chocolate", size = 1) +
#   ggplot2::geom_text(ggplot2::aes(label = label), nudge_y = -0.25,
#                      color = "navyblue") +
#   ggplot2::scale_color_manual(values = c("FALSE" = "darkgreen",
#                                          "TRUE" = "violetred")) +
#   ggplot2::scale_shape_manual(values = c("FALSE" = 19,
#                                          "TRUE" = 18)) +
#   ggplot2::scale_size_manual(values = c("TRUE" = 5, "FALSE" = 4)) +
#   ggplot2::coord_cartesian(ylim = the_ylim) +
#   ggplot2::theme_light() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::theme(title = ggplot2::element_text(color = "midnightblue")) +
#   the_labs
# p



# plot iris example -------------------------------------------------------


# data(iris)
# df <- iris
# out <- prune_fence(df, cols = c("Sepal.Width", "Petal.Width"),
#                    is_offset = TRUE, info = TRUE)
# df_plot <- out$results
# df_plot$label <- paste0("(", df_plot$x, ", ", df_plot$y, ")")
# df_plot$label[!out$is_oob] <- NA_character_

# the plot
# the_ylim <- c(NA_real_, ceiling(max(df_plot$y)))
# the_subtitle <- sprintf("%d inside the fence, %d outside the fence.",
#                         sum(!df_plot$oob), sum(df_plot$oob))
# the_labs <- ggplot2::labs(
#   title = "Iris data with a fence",
#   subtitle = the_subtitle,
#   x = "Sepal.Width", y = "Petal.Width"
# )
# p <- ggplot2::ggplot(data = df_plot, ggplot2::aes(x = x, y = y)) +
#   ggplot2::geom_point(ggplot2::aes(color = oob, shape = oob, size = oob)) +
#   ggplot2::geom_line(ggplot2::aes(y = small),
#                      linetype = "dotdash", color = "chocolate", size = 1) +
#   ggplot2::geom_line(ggplot2::aes(y = big),
#                      linetype = "dotdash", color = "chocolate", size = 1) +
#   ggplot2::geom_text(ggplot2::aes(label = label), nudge_y = -0.1,
#                      color = "navyblue", size = 3) +
#   ggplot2::scale_color_manual(values = c("FALSE" = "darkgreen",
#                                          "TRUE" = "violetred")) +
#   ggplot2::scale_shape_manual(values = c("FALSE" = 19,
#                                          "TRUE" = 18)) +
#   ggplot2::scale_size_manual(values = c("TRUE" = 3, "FALSE" = 2)) +
#   ggplot2::coord_cartesian(ylim = the_ylim) +
#   ggplot2::theme_light() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::theme(title = ggplot2::element_text(color = "midnightblue")) +
#   the_labs
# p
