#' Add Impossible Values to Data
#'
#' This function replaces a proportion of entries in integer columns
#' whose names begin with `"X"` with a specified impossible value.
#' This can be used to simulate data entry errors or invalid responses
#' in teaching datasets.
#'
#' @param .data A data frame. Assumed to contain integer columns, some of which
#'   start with `"X"`.
#' @param prop A numeric value between 0 and 1 giving the proportion of values
#'   to replace (default: 0.05).
#' @param replacement_value An integer to insert as the impossible value (default: 99).
#'
#' @return A modified data frame where a random proportion of values in
#'   columns beginning with `"X"` have been replaced with `value`.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   id = 1:5,
#'   X1 = sample(1:5, 5, replace = TRUE),
#'   X2 = sample(1:5, 5, replace = TRUE)
#' )
#' dirt_impossible_values(df, prop = 0.2, value = 99)
#' }
#'
#' @export
#' @importFrom dplyr across mutate where
#' @importFrom stats runif
dirt_impossible_values <- function(.data, prop = 0.03, replacement_value = 99L) {
  mutate(.data, 
         across(starts_with("X"), ~ replace(.x, runif(length(.x)) < prop, replacement_value)))
}