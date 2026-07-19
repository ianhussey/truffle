#' truffle: Create Teaching Datasets with Known Effects Hidden in Messy Data
#'
#' Generate datasets with known statistical effects (truffles) that students
#' must rediscover, then bury them in realistic data-processing challenges
#' (dirt) that must be cleaned first. See [truffle_likert()] for the main data
#' generator and the package README and vignette for worked examples:
#' `vignette("truffle", package = "truffle")`.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Column names referenced via non-standard evaluation inside dplyr/tidyr verbs.
# Declaring them avoids spurious "no visible binding for global variable" NOTEs
# from R CMD check.
utils::globalVariables(c("age", "condition", "gender", "id", "score"))
