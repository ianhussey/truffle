#' Format p-values in APA style
#'
#' Produces APA-style formatted p-values with a flexible number of decimals.
#' Always removes the leading zero before the decimal separator.
#'
#' @param p Numeric vector of p-values.
#' @param digits Integer. Number of decimal places to display (default = 3).
#'   The lower-bound threshold is set at 10^(-digits), e.g. with
#'   `digits = 3`, values smaller than .001 are reported as `< .001`.
#' @param decimal_separator Character. Decimal separator to use (default = ".").
#'   Use `","` for locales that prefer a comma.
#'
#' @details
#' - Leading zeros before the decimal separator are always removed
#'   (e.g., `0.023` → `.023`).
#' - Values below the reporting threshold are displayed as
#'   `< .00X` depending on `digits`.
#' - Values greater than 1 are capped at 1.000 (or the chosen digits).
#' - `NA` inputs are returned as `NA_character_`.
#'
#' @return A character vector of formatted p-values.
#'
#' @examples
#' round_p_value(c(0.023, 0.0004, 0.5))
#' round_p_value(c(0.023, 0.00004, 0.5), digits = 4)
#' round_p_value(0.023, digits = 3, decimal_separator = ",")
#'
#' @export
round_p_value <- function(p, digits = 3, decimal_separator = ".") {
  # coerce
  p <- as.numeric(p)
  thresh <- 10^(-digits)
  
  # helper: escape separator for regex
  sep <- decimal_separator
  sep_esc <- if (sep == ".") "\\." else gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", sep)
  
  fmt_no_leading_zero <- function(x) {
    out <- formatC(x, format = "f", digits = digits)
    if (sep != ".") out <- gsub("\\.", sep, out)
    sub(paste0("^0(?=", sep_esc, ")"), "", out, perl = TRUE)
  }
  
  ifelse(
    is.na(p), NA_character_,
    ifelse(
      p < thresh,
      paste0("< ", fmt_no_leading_zero(thresh)),
      fmt_no_leading_zero(pmin(p, 1)) # cap at 1 for display
    )
  )
}