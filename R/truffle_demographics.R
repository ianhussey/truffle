#' Add Clean Demographics Columns
#'
#' This function augments a data frame with simulated demographic variables
#' (`age`, `gender`) using only clean entries. Ages are numeric values sampled
#' uniformly from `age_min` to `age_max`. Genders are sampled from the names of
#' `gender_probs` according to their probabilities.
#'
#' @param .data A data frame. Should contain an `id` column if you want `age` and
#'   `gender` placed after it.
#' @param gender_probs A **named** numeric vector giving probabilities for gender
#'   categories (e.g., `c(male = 0.30, female = 0.70)` or
#'   `c(male = .29, female = .68, nonbinary = .03)`). Will be normalized to sum to 1.
#' @param age_min,age_max Integer bounds for the uniform age draw (default 18–45).
#'
#' @return A data frame containing all original columns of `.data`, plus two new
#'   columns:
#'   \describe{
#'     \item{age}{Integer values sampled uniformly between \code{age_min} and \code{age_max}.}
#'     \item{gender}{Character values drawn from the names of \code{gender_probs}.}
#'   }
#'   If `id` exists, columns are ordered `id`, `age`, `gender`, then the remaining columns.
#'   If `id` is absent, `age` and `gender` are moved to the front.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(id = 1:5)
#' truffle_demographics(df, gender_probs = c(male = .4, female = .6))
#' }
#'
#' @export
#' @importFrom dplyr mutate relocate
truffle_demographics <- function(
    .data,
    gender_probs = c(male = 0.29, female = 0.68, nonbinary = 0.03),
    age_min = 18L,
    age_max = 45L
) {
  stopifnot(is.data.frame(.data))
  n <- nrow(.data)
  
  # ---- checks for age bounds ----
  stopifnot(is.numeric(age_min), is.numeric(age_max), age_min <= age_max)
  rng <- as.integer(seq(from = age_min, to = age_max))
  
  # ---- checks for gender_probs ----
  if (is.null(names(gender_probs)) || any(names(gender_probs) == "")) {
    stop("`gender_probs` must be a *named* numeric vector (e.g., c(male=.3, female=.7)).")
  }
  if (any(gender_probs < 0)) stop("`gender_probs` must be non-negative.")
  if (sum(gender_probs) == 0) stop("`gender_probs` must not sum to 0.")
  gp <- gender_probs / sum(gender_probs)
  
  # ---- generate variables ----
  age_vec    <- sample(rng, size = n, replace = TRUE)
  gender_vec <- sample(names(gp), size = n, replace = TRUE, prob = gp)
  
  # ---- assemble & order ----
  out <- dplyr::mutate(.data, age = age_vec, gender = gender_vec)
  
  if ("id" %in% names(out)) {
    out <- dplyr::relocate(out, id, .before = 1)
    out <- dplyr::relocate(out, age, .after = id)
    out <- dplyr::relocate(out, gender, .after = age)
  } else {
    # no id; place age then gender at the front
    out <- dplyr::relocate(out, age, .before = 1)
    out <- dplyr::relocate(out, gender, .after = age)
  }
  
  out
}
