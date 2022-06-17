#' Identify Rows for Prunning
#'
#' Identify rows for prunning.
#'
#' Set the column \code{prune_var} to \code{id} if the condition defined by
#' \code{func} is met. See more details in the details section of \code{prune}.
#'
#'
#' @inheritParams prune
#'
#' @seealso prune
#'
#' @return Data.frame with updated \code{prune_var} column.
#' @export
prune_proc <- function(data, id = NA_character_, prune_var = "prune_id",
                       func = NULL, cols, ...) {
  # at this point we should hav func !- NULL and id != NA
  # and cols should be a subset of names(data)
  checkmate::assertDataFrame(data, min.cols = 2, min.rows = 1)
  checkmate::assertString(id, min.chars = 1 , na.ok = FALSE, null.ok = FALSE)
  checkmate::assertNames(prune_var, subset.of = names(data))
  checkmate::assertFunction(func, null.ok = FALSE)
  checkmate::assertNames(cols, subset.of = names(data))
  checkmate::assertNames(id, disjunct.from = cols)  # id must not be in cols
  # validate function name
  func_name <- deparse(substitute(func))
  func_choices <- c("identity", "prune_oob_num", "prune_oob_date",
                    "prune_fence")
  checkmate::assertChoice(x = func_name, choices = func_choices)


  # rows todo are the ones with id or NA
  # NOTE: Do not use  != id as NA create perror with data.frame below
  is_todo <- data[, prune_var] == id | is.na(data[, prune_var])

  if (grepl(pattern = "identity", x = func_name, ignore.case = TRUE)) {
    # identity does nothing
    is_select <- rep.int(FALSE, times = nrow(data))
  } else {
    is_select <- func(data, cols = cols, ...)
  }
  assertthat::assert_that(length(is_select) == nrow(data))

  # do not change the rows that are not to do
  is_select <- is_select & is_todo

  # there must be no more selected rows than rows to do
  assertthat::assert_that(sum(is_select) <= sum(is_todo))

  # reset all rows to do to NA
  data[is_todo, prune_var] <- NA_character_
  # update the prune_var column
  data[is_select, prune_var] <- id
  data  # must return data explicitly to avoid error message on list
}
