#' Format numeric columns with thousands separators and optional unit suffix
#'
#' Takes one or more numeric columns (e.g., reaction times in milliseconds) 
#' and converts them to character with thousands separators and optional units 
#' (e.g., "1,234 ms").
#'
#' @param .data A data frame.
#' @param cols Columns to format (tidyselect). Can be a single column name or a vector.
#' @param big_mark Character used as thousands separator (default: ",").
#' @param decimal_mark Character used for decimal mark (default: ".").
#' @param unit Character suffix to append (default: " ms").
#'
#' @return A data frame with the specified columns converted to character.
#' @export
#' @importFrom dplyr mutate across all_of
#' @importFrom tidyselect all_of
dirt_numbers <- function(.data,
                         cols,
                         big_mark = ",",
                         decimal_mark = ".",
                         unit = " ms") {
  stopifnot(is.data.frame(.data))
  
  .data |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::all_of(cols),
        .fns = ~ {
          if (!is.numeric(.x)) stop("Selected column is not numeric.")
          formatted <- formatC(
            .x,
            big.mark = big_mark,
            decimal.mark = decimal_mark,
            format = "f",
            digits = 0
          )
          paste0(formatted, unit)
        }
      )
    )
}
