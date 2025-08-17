#' Add Missingness to Data
#'
#' This function randomly replaces a proportion of entries in integer columns
#' whose names begin with `"X"` with `NA`, simulating missing data.
#'
#' @param dat A data frame. Assumed to contain integer columns, some of which
#'   start with `"X"`.
#' @param prop A numeric value between 0 and 1 giving the proportion of values
#'   to replace with `NA` (default: 0.05).
#'
#' @return A modified data frame where a random proportion of values in
#'   columns beginning with `"X"` have been replaced with `NA`.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   id = 1:5,
#'   X1 = sample(1:5, 5, replace = TRUE),
#'   X2 = sample(1:5, 5, replace = TRUE)
#' )
#' add_missingness(df, prop = 0.2)
#' }
#'
#' @export
#' @importFrom dplyr across mutate where
#' @importFrom stats runif
add_missingness <- function(dat, proportion_missing){ 
  delete_MCAR(
    dat,
    p = proportion_missing,
    cols_mis = vars_select(colnames(dat), starts_with("X"))
  )
}