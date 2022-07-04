#' Create Example of Pruned Data with Iris
#'
#' Create an axample of pruned data with iris.
#'
#' See the \code{prune_fence} for more details. See \code{iris} for more
#' details on the original data.
#'
#' @return Dataframe
make_iris_pruned <- function() {
  df <- iris

  the_pruned <- c(c("oob" = 1,"out" = 50,"maha" = 100,
                    "oob" = 101, "out" = 150))

  df$prune_id <- NA_character_
  df$prune_id[the_pruned] <- names(the_pruned)
  df
}

# iris_prunned <- make_iris_pruned()
# usethis::use_data(iris_pruned, overwrite = TRUE)
