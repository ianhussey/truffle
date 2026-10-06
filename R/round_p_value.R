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
#' @param alpha Numeric or `NULL`. Significance threshold that rounding must
#'   not cross (default = .05). When rounding to `digits` would put a value on
#'   the other side of `alpha` (e.g. `.0499` → `.050`), extra decimal places
#'   are added until it no longer does (`.0499`). Use `NULL` to disable.
#'
#' @details
#' - Leading zeros before the decimal separator are always removed
#'   (e.g., `0.023` → `.023`).
#' - Values are rounded half up (e.g., `.0445` → `.045`), with a small
#'   tolerance so that floating-point representation error does not flip the
#'   result (`0.1235` is stored as `0.12349999...` but is rounded to `.124`).
#'   This follows the same approach as `roundwork::round_up()`. Because
#'   rounding can move a value across a significance threshold, extra digits
#'   are shown where needed to keep it on the correct side of `alpha`.
#' - Values below the reporting threshold are displayed as
#'   `< .00X` depending on `digits`.
#' - Values that would round to 1 (including exactly 1) are displayed as
#'   `> .999` depending on `digits`, so a p-value is never reported as 1.
#' - Values outside \[0, 1\] are an error.
#' - `NA` inputs are returned as `NA_character_`.
#'
#' @return A character vector of formatted p-values.
#'
#' @examples
#' round_p_value(c(0.023, 0.0004, 0.5))
#' round_p_value(c(0.023, 0.00004, 0.5), digits = 4)
#' round_p_value(0.023, digits = 3, decimal_separator = ",")
#' round_p_value(c(0.0499, 0.04996, 0.0501))
#' round_p_value(0.0499, alpha = NULL)
#' round_p_value(c(0.9994, 0.9996, 1))
#'
#' @export
round_p_value <- function(p, digits = 3, decimal_separator = ".", alpha = .05) {
  # coerce
  p <- as.numeric(p)
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("`p` must be between 0 and 1.", call. = FALSE)
  }
  thresh <- 10^(-digits)

  # helper: escape separator for regex
  sep <- decimal_separator
  sep_esc <- if (sep == ".") {
    "\\."
  } else {
    gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", sep)
  }

  # round half up, tolerant of floating-point error (as roundwork::round_up()),
  # rather than letting formatC() round the stored binary value
  round_half_up <- function(x, d) {
    p10 <- 10^d
    floor(x * p10 + 0.5 + .Machine$double.eps^0.5 / 10) / p10
  }

  # digits needed so that rounding doesn't move a value across alpha
  digits_needed <- function(x) {
    d <- digits
    if (is.null(alpha) || is.na(x)) return(d)
    while (d < 15 && (round_half_up(x, d) < alpha) != (x < alpha)) {
      d <- d + 1
    }
    d
  }

  fmt_no_leading_zero <- function(x) {
    d <- vapply(x, digits_needed, numeric(1))
    out <- mapply(
      function(xi, di) formatC(round_half_up(xi, di), format = "f", digits = di),
      x, d,
      USE.NAMES = FALSE
    )
    if (sep != ".") {
      out <- gsub("\\.", sep, out)
    }
    sub(paste0("^0(?=", sep_esc, ")"), "", out, perl = TRUE)
  }

  ifelse(
    is.na(p),
    NA_character_,
    ifelse(
      p < thresh,
      paste0("< ", fmt_no_leading_zero(thresh)),
      ifelse(
        round_half_up(p, digits) >= 1,
        paste0("> ", fmt_no_leading_zero(1 - thresh)),
        fmt_no_leading_zero(p)
      )
    )
  )
}
