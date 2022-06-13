#' Reset the Prune ID Column
#'
#' Reset the prune id column.
#'
#' If it doesn't already exists it will create the prune id column and set
#' to \code{NA}.
#'
#' @param df dataframe.
#' @param prune_var String, name of the column with the prune id.
#' @param id String. If \code{NA} (default) the whole column will be reset to
#' \code{NA}. if not \code{NA} then only the element with \code{id} will
#' be reset to \code{NA}.
#'
#' @return dataframe with reset \code{prune_var}.
#' @export
#'
#' @examples
#' df <- data.frame(alpha = letters[1:3])
#' df <- prune_reset(df, prune_var = "prune_id")
prune_reset <- function(df, prune_var = "prune_id", id = NA_character_) {
  checkmate::assertDataFrame(df, min.cols = 1, min.rows = 1)
  checkmate::assertNames(prune_var, type = "strict")
  checkmate::assertString(id, na.ok = TRUE, null.ok = FALSE)

  if (is.na(id)) {
    df <- df %>% mutate(!!prune_var := factor(NA_character_))
  } else {
    df <- df %>%
      mutate(!!prune_var := replace(x = .data[[prune_var]],
                                    list = .data[[prune_var]] == id,
                                    values = NA_character_))

    # even though the factor is set to NA, it must be kept in the levels
    # to avoid problem when the factor is required again later.
    # For example when doing split/unsplit.
    df <- df %>%
      mutate(!!prune_var := forcats::fct_expand(f = .data[[prune_var]], id))
  }

  df
}


#' Update the prune id
#'
#' Update the prune id
#'
#' Set the prune id based on a logical vector to \code{id}
#'
#' @param df dataframe.
#' @param cond logical vector where TRUE = update the id when it is not already
#'  set, that is if its value ids not \code{id}.
#' @param prune_var String, name of the column with the prune id
#' @param id String, identification of the pruning in \code{prune_var}.
#' It must be different than  \code{NA_character_}.
#'
#' @return dataframe with update column \code{prune_var}.
#' @export
#'
#' @examples
#' df <- data.frame(
#'  alpha = letters[1:3],
#'  prune_id = factor(c(NA_character_, "out", NA_character_))
#'  )
#' df <- prune_upd(df, cond = c(FALSE, TRUE, TRUE), id = "example")
#' # this should update the third element to "example"
#' stopifnot(all.equal(df$prune_id, factor(c(NA_character_, "out", "example")),
#'  check.attributes = FALSE))
prune_upd <- function(df, cond, prune_var = "prune_id", id) {

  checkmate::assertDataFrame(df, min.cols = 2, min.rows = 1)
  checkmate::assertLogical(cond, len = nrow(df))
  checkmate::assertNames(names(df), must.include = prune_var)
  checkmate::assertString(id, min.chars = 1, na.ok = FALSE, null.ok = FALSE)


  check <- sum(is.na(df[, prune_var]))
  if (check) {
    # add the new factor, forcats::fct_expand() will, silently,
    df <- df %>%
      mutate(!!prune_var := forcats::fct_expand(f = .data[[prune_var]], id))
  } else {
    msg_head <- cli::col_yellow("No row has the default prune id.")
    msg_body <- c("i" = "Usually indicates something wrong with the prune id.")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_upd_error2",
      val = id
    )
  }

  # You cannot have NA in the cond vector
  check <- sum(is.na(cond))
  if (check) {
    msg_head <- cli::col_yellow("Some prune conditions are NA:")
    msg_body <- c("x" = sprintf("Nb of NA in prune condition = %d", check))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prune_upd_error3",
      val = check
    )
  }

  # update the condition to update only where prune_id is NA
  cond_final <- cond & is.na(df[, prune_var])
  df %>% mutate(!!prune_var := replace(x = .data[[prune_var]],
                                       list = cond_final, values = id))
}

#' Update the prune flag
#'
#' Update the prune flag.
#'
#' Update the prune flag by setting it to TRUE when the string in
#' \code{prune_var} is not equal to \code{NA_character_} and set it to FALSE
#' otherwise.
#'
#' @param df dataframe
#' @param prune_var  String, name of the column with the prune id
#' @param flag_var  String, name of the column with the prune flag
#'
#' @return dataframe with update column \code{prune_flag}
#' @export
#'
#' @examples
#' df <- data.frame(
#'  alpha = letters[1:3],
#'  prune_id = factor(c(NA_character_, "out", NA_character_)),
#'  is_pruned = FALSE)
#' df <- prune_flag(df)
#' # this should give the following updated dataframe
#' target <- data.frame(
#'  alpha = letters[1:3],
#'  prune_id = factor(c(NA_character_, "out", NA_character_)),
#'  is_pruned = c(FALSE, TRUE, FALSE))
#' stopifnot(identical(df, target))
# prune_flag <- function(df, prune_var = "prune_id", flag_var = "is_pruned") {
#   checkmate::assertDataFrame(df, min.cols = 2, min.rows = 1)
#   checkmate::assertNames(names(df), must.include = prune_var)
#
#   df %>% mutate(!!flag_var := !is.na(.data[[prune_var]]))
# }
