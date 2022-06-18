#' Create Example Dataframe to Use With Fencing Algorithm
#'
#' Create example dataframe to use with fencing algorithm.
#'
#' See the \code{prune_fence} for more details.
#'
#' @return Dataframe
make_fences_df <- function() {
  df <- data.frame(
    X = c(0.0, 0.0, 0.3, 1.0, 3.0, 3.0, 3.0, 7.5, 8.0, 8.2),
    Y = c(0.0, 0.5, 6.0, 2.0, 0.0, 2.5, 7.5, 1.0, 0.51, 6.0),
    prune_id = NA_character_,
    oob = c(F, T, T, F, T, F, F, F, F, F)
  )
  df$label <- paste0("(", df$X, ", ", df$Y, ")")
  df
}

# fences_df <- make_fences_df()
# usethis::use_data(fences_df, overwrite = TRUE)

#' Create Example Dataframe With NA to Use With Fencing Algorithm
#'
#' Create example dataframe with NA to use with fencing algorithm.
#'
#' See the \code{prune_fence} for more details.
#'
#' @return Dataframe
make_fencesNA_df <- function() {
  df <- data.frame(
    X = c(0.0, 0.0, 0.3, 1.0, 3.0, 3.0, 3.0, 7.5, 8.0, 8.2,
          0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, NA_real_, 5.5),
    Y = c(0.0, 0.5, 6.0, 2.0, 0.0, 2.5, 7.5, 1.0, 0.5, 6.0,
          0.5, 1.7, 1.8, 2.3, 3.2, 3.2, NA_real_, 4.1, 5.4, 5.5),
    prune_id = NA_character_,
    oob = c(F, T, T, F, T, F, F, F, F, F,
            F, F, F, F, F, F, T, F, T, F)
  )
  df$label <- paste0("(", df$X, ", ", df$Y, ")")
  df
}

# fencesNA_df <- make_fencesNA_df()
# usethis::use_data(fencesNA_df, overwrite = TRUE)
