#' Identify Rows for Prunning
#'
#' Identify rows for prunning.
#'
#' Set the column \code{prune_var} to \code{id} if the condition defined by
#' \code{func} is met.
#'
#' @section prune:
#'
#' When \code{func = NULL} and \code{id = NA_character_}, then the column
#' \code{prune_var} will be reset to \code{NA}. If the column \code{prune_var}
#' does not exist it will be created.  This is used usually to create the
#' \code{prune_var} column. It can also be used to reset it if it already exists.
#'
#' When \code{func = NULL} and \code{id != NA_character}, then an error message
#' will be issued as an \code{id} cannot exist without a corresponding function.
#'
#' When \code{func != NULL} and \code{id != NA_character}, then all elements
#' of column \code{prune_var} selected by \code{func} will be set to \code{id}
#' \emph{if they are NA or already set to id}, that is the rows with a
#' pre-existing value other than \code{id} will be kept as is.  This is
#' necessary to ensure that previous id are kept which is very useful when some
#' id are subsets of others be must be kept. For example when one wants to
#' keep zeros be exclude other values in a range then we would prune for zeros
#' \emph{then prune for the range}.
#'
#' When \code{func != NULL} and \code{id = NA_character}, then an error message
#' will be issued as a function must always have a corresponding \code{id}.
#'
#'
#' @param data Dataframe.
#' @param id String. ID used to identify the rows in column \code{prune_var}.
#' @param prune_var The name of the column with the prune id. Default is
#' \code{prune}.
#' @param func Function used to select the rows meeting the condition for
#' prunning. Default is \code{NULL}. If \code{identity()} is used, nothing will
#' be done.
#' @param cols Names of columns to apply \code{func} to. Must be a character
#' vector with a minimum length of one.
#' @param ... Additional arguments used by \code{func}.
#'
#' @seealso prune_proc
#'
#' @return Dataframe with updated \code{prune_var} column.
#' @export
prune <- function(data, id = NA_character_, prune_var = "prune_id", func = NULL,
                  cols, ...) {
  checkmate::assertDataFrame(data, min.cols = 1, min.rows = 1)
  checkmate::assertString(id, min.chars = 1 , na.ok = TRUE, null.ok = FALSE)
  checkmate::assertNames(prune_var, type = "strict")
  checkmate::assertFunction(func, null.ok = TRUE)

  # You cannot have func = NULL and id != NA_character
  check <- is.null(func) & !is.na(id)
  if (check) {
    msg_head <- cli::col_yellow("The id must be NA when the function is NULL.")
    msg_body <- c("i" = sprintf("The value of id: %s", id))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_error1",
      val = check
    )
  }

  # You cannot have func != NULL and id = NA_character
  check <- !is.null(func) & is.na(id)
    if (check) {
      msg_head <- cli::col_yellow("An id must be given with the function.")
      msg_body <- c("i" ="The function is not NULL.")
      msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
      rlang::abort(
        message = msg,
        class = "prune_error2",
        val = check
      )
    }

  # If func = NULL and id = NA then reset prune_id to NA
  if (is.null(func) & is.na(id)) {
    out <- data |>
      mutate(!!prune_var := NA_character_)
    return(out)
  }

  # at this point we must have func !- NULL and id != NA
  # and cols should be a subset of names(data)
  prune_proc(data = data, id = NA_character_, prune_var = "prune_id",
             func = func, cols = cols, ...)
}
