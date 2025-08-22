#' Check if a vector is integer-like (whole numbers or NA)
#'
#' @param x A numeric vector.
#' @return A single logical: `TRUE` if all non-missing values are whole numbers.
#' @keywords internal
.is_integer_like <- function(x) {
  all(is.na(x) | x == floor(x))
}

#' Validate that all columns are numeric and integer-like
#'
#' Aborts with a clear message if any column is non-numeric or contains
#' non-integer values (excluding `NA`).
#'
#' @param df A data frame/tibble of item columns.
#' @param context Character label used in error messages (e.g., "scoring step").
#' @return Invisibly returns `NULL`; called for its side effects (validation).
#' @keywords internal
.check_numeric_integer_like <- function(df, context = "Likert items") {
  if (!all(purrr::map_lgl(df, is.numeric))) {
    rlang::abort(paste0("Non-numeric values detected in ", context, "."))
  }
  bad_vars <- names(df)[!purrr::map_lgl(df, ~ all(is.na(.x) | .x == floor(.x)))]
  if (length(bad_vars) > 0L) {
    rlang::abort(paste0(
      "Non-integer values detected in ", context, ": ",
      paste(bad_vars, collapse = ", "), "."
    ))
  }
}

#' Select scale variables by prefix or regex
#'
#' @param .data A data frame or tibble.
#' @param id Character; prefix (default) or regex pattern for selecting columns.
#' @param regex Logical; if `TRUE`, treat `id` as a regex pattern; otherwise as a prefix.
#' @return A character vector of matching column names.
#' @details Aborts if no columns match.
#' @keywords internal
.select_scale_vars <- function(.data, id, regex = FALSE) {
  stopifnot(is.data.frame(.data))
  stopifnot(is.character(id), length(id) == 1)
  stopifnot(is.logical(regex), length(regex) == 1)
  
  vars <- if (regex) {
    names(.data)[stringr::str_detect(names(.data), id)]
  } else {
    names(.data)[stringr::str_starts(names(.data), id)]
  }
  
  if (length(vars) == 0L) {
    rlang::abort(paste0("No columns matched '", id, "'."))
  }
  vars
}

#' Flag impossible values (outside the bounds \[min, max\]) for selected items
#'
#' @param .data A data frame or tibble.
#' @param vars Character vector of column names to check.
#' @param min,max Integer-like bounds (inclusive).
#' @return A list with two elements:
#' \describe{
#'   \item{n_imp}{Integer vector: row-wise count of impossible values.}
#'   \item{imp_items}{Character vector: row-wise comma-separated names with impossible values.}
#' }
#' @details Validates that all columns are numeric and integer-like.
#' @keywords internal
.flag_impossible <- function(.data, vars, min, max) {
  df <- dplyr::select(.data, dplyr::all_of(vars))
  .check_numeric_integer_like(df, "impossible-value check")
  
  imp_mat <- df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ !dplyr::between(.x, min, max) & !is.na(.x)))
  
  n_imp <- rowSums(imp_mat)
  imp_list <- purrr::pmap_chr(imp_mat, function(...) {
    row_vals <- c(...)
    bads <- names(row_vals)[row_vals]
    if (length(bads) == 0L) "" else paste(bads, collapse = ", ")
  })
  
  list(n_imp = n_imp, imp_items = imp_list)
}

#' Clean impossible values by setting them to NA
#'
#' @param .data A data frame or tibble.
#' @param vars Character vector of columns to clean.
#' @param min,max Integer-like bounds (inclusive).
#' @return The input data with selected columns cleaned (out-of-range values set to `NA_real_`).
#' @details Validates that all selected columns are numeric and integer-like.
#' @keywords internal
.clean_impossible <- function(.data, vars, min, max) {
  df <- dplyr::select(.data, dplyr::all_of(vars))
  .check_numeric_integer_like(df, "cleaning step")
  
  dplyr::mutate(
    .data,
    dplyr::across(dplyr::all_of(vars), ~ ifelse(dplyr::between(.x, min, max), .x, NA_real_))
  )
}

#' Reverse-score selected variables
#'
#' @param .data A data frame or tibble.
#' @param vars_to_reverse Character vector of column names to reverse.
#' @param min,max Integer-like bounds (inclusive).
#' @return The data with `vars_to_reverse` reverse-scored as `(min + max) - x`.
#' @details If `vars_to_reverse` is empty, returns `.data` unchanged. Validates numeric and integer-like.
#' @keywords internal
.reverse_score_vars <- function(.data, vars_to_reverse, min, max) {
  if (length(vars_to_reverse) == 0L) return(.data)
  df <- dplyr::select(.data, dplyr::all_of(vars_to_reverse))
  .check_numeric_integer_like(df, "reverse-scoring step")
  
  dplyr::mutate(
    .data,
    dplyr::across(
      dplyr::all_of(vars_to_reverse),
      ~ ifelse(is.na(.x), NA_real_, (min + max) - .x)
    )
  )
}

#' Compute row-wise sum scores and non-missing counts
#'
#' @param .data A data frame or tibble.
#' @param vars Character vector of item columns to score.
#' @param sum_name,n_name,items_name Character names for new columns:
#'   sum score, non-missing count, and item list.
#' @return The input data with three new columns:
#'   `n_name` (non-missing count), `sum_name` (sum over available items; `NA` if count==0),
#'   and `items_name` (comma-separated list of `vars`).
#' @details Validates numeric and integer-like; returns `NA` (not `NaN`) when all items are missing.
#' @keywords internal
.compute_sum_and_counts <- function(.data, vars, sum_name, n_name, items_name) {
  df <- dplyr::select(.data, dplyr::all_of(vars))
  .check_numeric_integer_like(df, "scoring step")
  
  .data |>
    dplyr::mutate(
      !!n_name := rowSums(!is.na(dplyr::pick(dplyr::all_of(vars)))),
      !!sum_name := ifelse(
        .data[[n_name]] == 0,
        NA_real_,
        rowMeans(dplyr::pick(dplyr::all_of(vars)), na.rm = TRUE) * .data[[n_name]]
      ),
      !!items_name := paste(vars, collapse = ", ")
    )
}


#' Score Likert-style items with cleaning, optional reverse-scoring, and diagnostics
#'
#' Cleans out-of-range values to `NA`, optionally reverse-scores a subset of items,
#' and computes row-wise **sum scores** and **non-missing item counts**, implicitly imputing
#' missing data from each participant's mean. Also returns diagnostics about impossible values 
#' observed in the raw data.
#'
#' @param .data A data frame or tibble of responses.
#' @param scale_identifier Character; prefix or regex used to select item columns (see `regex`).
#' @param min,max Integer-like lower/upper bounds (inclusive) for valid responses.
#' @param id_col Optional character: name of a unique identifier column to check for duplicates.
#' @param regex Logical; if `TRUE`, treat `scale_identifier` as a regex; otherwise as a prefix.
#' @param output_prefix Optional character used to name derived columns. Defaults to `scale_identifier`.
#' @param reverse Optional character vector of item names to reverse, or a regex pattern
#'   when `reverse_regex = TRUE`.
#' @param reverse_regex Logical; if `TRUE`, interpret `reverse` as a regex over selected items.
#'
#' @return A tibble containing the original data plus:
#' \itemize{
#'   \item `<prefix>sumscore` — sum score over available items (`NA` if all missing)
#'   \item `<prefix>nonmissing_n` — count of non-missing items per row.
#'   \item `<prefix>complete_data` — are all items per row non-missing.
#'   \item `<prefix>items` — comma-separated list of items included.
#'   \item `<prefix>items_reversed` — comma-separated list of reversed items (constant per row).
#'   \item `<prefix>impossible_n` — row-wise count of impossible values observed pre-cleaning.
#'   \item `<prefix>impossible_items` — row-wise comma-separated list of items with impossible values.
#' }
#'
#' @details
#' **Processing order:** diagnostics on raw data → clean impossible values → reverse-score →
#' compute sums and counts → attach diagnostics. All selected items must be numeric and integer-like.
#' A single common `[min, max]` is assumed across all selected items.
#' If `id_col` is supplied, the function aborts on duplicate IDs.
#'
#' @examples
#' \dontrun{
#' dat <- tibble::tibble(
#'   id = 1:3,
#'   bfi_1 = c(1, 2, 6),  # 6 is invalid -> NA
#'   bfi_2 = c(3, NA, 4),
#'   bfi_3 = c(5, 4, 2)
#' )
#' 
#' snuffle_sum_scores(dat, "bfi_", min = 1, max = 5, id_col = "id")
#' }
#'
#' @export
snuffle_sum_scores <- function(.data,
                               scale_identifier,
                               min, max,
                               id_col = NULL,
                               regex = FALSE,
                               output_prefix = NULL,
                               reverse = NULL,
                               reverse_regex = FALSE) {
  stopifnot(is.numeric(min), is.numeric(max), min < max)
  
  if (!inherits(.data, "data.frame")) {
    rlang::abort("`.data` must be a rectangular data structure coercible to a data frame.")
  }
  .data <- as_tibble(.data)
  
  # ID checks (optional)
  if (!is.null(id_col)) {
    if (!id_col %in% names(.data)) {
      rlang::abort(paste0("ID column `", id_col, "` not found in data."))
    }
    if (anyDuplicated(.data[[id_col]]) > 0) {
      dupes <- .data[[id_col]][duplicated(.data[[id_col]])]
      rlang::abort(paste0("Duplicate IDs detected in `", id_col, "`: ",
                          paste(head(unique(dupes)), collapse = ", "),
                          if (length(unique(dupes)) > 5) " \u2026"))
    }
  }
  
  vars <- .select_scale_vars(.data, scale_identifier, regex)
  
  # Output names
  prefix        <- output_prefix %||% scale_identifier
  sum_col       <- paste0(prefix, "sumscore")
  n_col         <- paste0(prefix, "nonmissing_n")
  items_col     <- paste0(prefix, "items")
  rev_items_col <- paste0(prefix, "items_reversed")
  imp_n_col     <- paste0(prefix, "impossible_n")
  imp_items_col <- paste0(prefix, "impossible_items")
  complete_col  <- paste0(prefix, "complete_data")
  
  # Diagnostics (before cleaning)
  diagnostics <- .flag_impossible(.data, vars, min, max)
  
  # Clean → reverse → score
  out <- .clean_impossible(.data, vars, min, max)
  
  vars_to_reverse <- character(0)
  if (!is.null(reverse)) {
    if (reverse_regex) {
      vars_to_reverse <- vars[stringr::str_detect(vars, reverse)]
    } else {
      vars_to_reverse <- intersect(vars, reverse)
    }
  }
  
  out <- .reverse_score_vars(out, vars_to_reverse, min, max)
  out <- .compute_sum_and_counts(out, vars, sum_col, n_col, items_col)
  
  # Number of items actually used for this scale
  n_items <- length(vars)
  
  # Attach diagnostics, reversed-items, and complete_data flag
  out <- out |>
    dplyr::mutate(
      !!complete_col  := .data[[n_col]] == n_items,
      !!rev_items_col := if (length(vars_to_reverse) == 0L) NA_character_
                         else paste(vars_to_reverse, collapse = ", "),
      !!imp_n_col     := diagnostics$n_imp,
      !!imp_items_col := dplyr::na_if(diagnostics$imp_items, "")
    )
  
  return(out)
}
