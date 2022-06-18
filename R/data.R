#' Example Dataframe to Use with Fencing Algorithm
#'
#' Example dataframe to use with fencing algorithm.
#'
#' An example with data points outside of the \emph{fence} and \code{NA} that
#' are processed with the fencing algorithm. See \code{prune_fence} function
#' for details.
#'
#' @docType data
#'
#' @format Dataframe with 10 rows and 5 variables.
#' \describe{
#'   \item{X}{x values.}
#'   \item{Y}{y values.}
#'   \item{prune_id}{prune id for fence, NA_character or oob_fence.}
#'   \item{oob}{FALSE if at or inside the fence, TRUE otherwise
#'   (i.e. out-of-bound).}
#'   \item{label}{Label. Usually used for plotting.}
#' }
#'
"fences_df"

#' Example Dataframe to Use with Fencing Algorithm
#'
#' Example dataframe to use with fencing algorithm.
#'
#' An example with data points outside of the \emph{fence} and \code{NA} that
#' are processed with the fencing algorithm. See \code{prune_fence} function
#' for details.
#'
#' @docType data
#'
#' @format Dataframe with 10 rows and 5 variables.
#' \describe{
#'   \item{X}{x values.}
#'   \item{Y}{y values.}
#'   \item{prune_id}{prune id for fence, NA_character or oob_fence.}
#'   \item{oob}{FALSE if at or inside the fence, TRUE otherwise
#'   (i.e. out-of-bound).}
#'   \item{label}{Label. Usually used for plotting.}
#' }
#'
"fencesNA_df"
