#' Add a "block_trial" Column to a Data Frame
#'
#' This helper function adds a new column (by default called `"block_trial"`) 
#' that combines a randomly sampled block number and a sequential trial number. 
#' The resulting column is a character vector in the format 
#' `"block<block>_trial<trial>"` (e.g., `"block3_trial005"`).
#'
#' @param dat A data frame.
#' @param col A string giving the name of the new column (default: `"block_trial"`).
#'
#' @details
#' - Block numbers are sampled uniformly from 1 to 6, with one block number 
#'   assigned to the entire data frame.
#' - Trial numbers are based on the row order of \code{dat} and zero-padded to 3 digits.
#' - The function uses \code{dplyr::mutate()} and non-standard evaluation (NSE) 
#'   to create the column.
#'
#' @return
#' A data frame equal to \code{dat} but with an additional column named \code{col}.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' dat <- data.frame(id = 1:5)
#' add_non_tidy_column(dat)
#' add_non_tidy_column(dat, col = "trial_id")
#' }
#' 
#' @export
add_non_tidy_column <- function(dat, col = "block_trial") {
  dplyr::mutate(dat, !!col := sprintf("block%d_trial%03d", sample(1:6, n(), 1), dplyr::row_number()))
}
