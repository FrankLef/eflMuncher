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
    is_oob = c(F, T, T, F, T, F, F, F, F, F)
  )
  df$label <- paste0("(", df$X, ", ", df$Y, ")")
  df
}

fences_df <- make_fences_df()
usethis::use_data(fences_df, overwrite = TRUE)
