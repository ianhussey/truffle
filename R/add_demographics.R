#' Add Clean Demographics Columns
#'
#' This function augments a data frame with simulated demographic variables
#' (`age`, `gender`) using only clean entries. Ages are numeric values sampled
#' uniformly from 18 to 45. Genders are sampled from `"male"` and `"female"`
#' according to specified probabilities.
#'
#' @param dat A data frame. Must contain at least one column (e.g., `id`).
#' @param gender_probs A named numeric vector giving probabilities for the base
#'   gender categories (default: `c(male = 0.30, female = 0.70)`). Must sum to 1.
#'
#' @return A data frame containing all original columns of `dat`, plus two new
#'   columns:
#'   \describe{
#'     \item{age}{Integer values sampled uniformly between 18 and 45.}
#'     \item{gender}{Character values `"male"` or `"female"` drawn from the
#'       provided probabilities.}
#'   }
#'   Columns are ordered with `id` first, followed by `age`, `gender`, then the
#'   remaining columns of `dat`.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(id = 1:5)
#' add_demographics(df)
#' }
#'
#' @export
#' @importFrom dplyr mutate relocate
#' @importFrom stats runif
add_demographics <- function(
    dat,
    gender_probs = c(male = 0.30, female = 0.70),
    age_min = 18L,
    age_max = 45L
) {
  stopifnot(is.data.frame(dat))
  n <- nrow(dat)
  
  # ---- checks for age bounds ----
  stopifnot(is.numeric(age_min), is.numeric(age_max), age_min <= age_max)
  rng <- as.integer(seq(from = age_min, to = age_max))
  
  # ---- checks for gender_probs ----
  # allow unnamed c(0.3, 0.7) (assumed c(male, female)) or named with any order
  if (is.null(names(gender_probs))) {
    if (length(gender_probs) != 2L) stop("gender_probs must have length 2 (male, female).")
    names(gender_probs) <- c("male", "female")
  }
  # keep only male/female (warn if others) and normalise to sum 1
  gp <- gender_probs[c("male", "female")]
  if (any(is.na(gp))) stop("gender_probs must include names 'male' and 'female'.")
  if (any(gp < 0))    stop("gender_probs must be non-negative.")
  if (sum(gp) == 0)   stop("gender_probs must not sum to 0.")
  gp <- gp / sum(gp)
  
  # ---- generate variables ----
  age_vec    <- sample(rng, size = n, replace = TRUE)              # integer ages
  gender_vec <- sample(names(gp), size = n, replace = TRUE, prob = gp)
  
  # ---- assemble & order ----
  dplyr::mutate(dat,
                age = age_vec,
                gender = gender_vec) |>
    dplyr::relocate(id, .before = 1) |>
    dplyr::relocate(age, .after = id) |>
    dplyr::relocate(gender, .after = age)
}