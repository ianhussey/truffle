#' Format numbers column with thousands separators and optional units suffix (e.g., "ms")
#'
#' Takes a numeric column of reaction times (milliseconds) and converts
#' them to strings like "1,234 ms".
#'
#' @param .data A numeric vector of reaction times in ms.
#' @param big_mark Character used as thousands separator (default: ",").
#' @param decimal_mark Character used for decimal mark (default: ".").
#' @param unit Character suffix to append (default: " ms").
#'
#' @return A character vector of formatted reaction times.
#' @export
dirt_numbers <- function(.data,
                         big_mark = ",",
                         decimal_mark = ".",
                         unit = " ms") {
  if (!is.numeric(.data)) {
    stop("`x` must be numeric (reaction times in ms).")
  }
  
  formatted <- formatC(
    .data,
    big.mark   = big_mark,
    decimal.mark = decimal_mark,
    format     = "f",
    digits     = 0
  )
  
  paste0(formatted, unit)
}
