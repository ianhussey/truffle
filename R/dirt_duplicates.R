#' Add Duplicate Rows
#'
#' This function randomly duplicates a proportion of rows in a data frame,
#' appending the duplicates to the end of the data. If an `id` column is present,
#' the resulting data will be sorted by `id`. It is useful for
#' demonstrating data cleaning steps such as deduplication.
#'
#' @param .data A data frame.
#' @param prop Numeric in \eqn{[0,1]}. Proportion of rows to duplicate
#'   (e.g., `0.1` duplicates 10% of rows). Default = 0.05.
#' @param replace Logical. If `TRUE`, rows can be duplicated multiple times.
#'   If `FALSE`, each row will be duplicated at most once. Default = TRUE.
#'
#' @return A data frame equal to `.data` with additional duplicated rows appended.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(id = 1:10, score = rnorm(10))
#' dirt_duplicates(df, prop = 0.2)
#' }
#'
#' @export
#' @importFrom dplyr bind_rows arrange
dirt_duplicates <- function(.data, prop = 0.05, replace = TRUE) {
  stopifnot(is.data.frame(.data))
  stopifnot(is.numeric(prop), length(prop) == 1, prop >= 0, prop <= 1)
  
  n <- nrow(.data)
  n_dup <- ceiling(n * prop)
  
  if (n_dup == 0) return(.data)
  
  dup_idx <- sample(seq_len(n), size = n_dup, replace = replace)
  dup_rows <- .data[dup_idx, , drop = FALSE]
  
  out <- dplyr::bind_rows(.data, dup_rows)
  
  if ("id" %in% colnames(out)) {
    out <- dplyr::arrange(out, id)
  }
  
  rownames(out) <- NULL
  out
}