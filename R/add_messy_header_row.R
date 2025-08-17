#' Add Undesirable Header Rows
#'
#' This function prepends arbitrary text rows above a data.frame's column names. 
#' When written to disk, these rows will appear before the actual header row, 
#' requiring the use of `readr::read_csv(skip = n)` to import cleanly.
#'
#' @param data A data.frame or tibble.
#' @param headers Character vector of header rows to prepend (e.g., dates, notes).
#'
#' @return A character matrix that can be written to disk with `readr::write_lines()` 
#'   or a similar function. Each header row is one line of text, followed by the CSV data.
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   id = 1:3,
#'   score = c(10, 20, 15)
#' )
#' messy <- add_header_rows(dat, c("Data collected: 2025-08-17", "Study: Pilot"))
#' readr::write_lines(messy, "messy.csv")
#' # To re-import: readr::read_csv("messy.csv", skip = 2)
#' }
add_messy_header_rows <- function(data, headers = c("Study: depression RCT", "Data collected: 2025-08-17")) {
  stopifnot(is.data.frame(data))
  
  # write main data to CSV string
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(data, tmp)
  csv_body <- readr::read_lines(tmp)
  
  # prepend headers
  out <- c(headers, csv_body)
  return(out)
}