#' Check Generated Data
#'
#' This function checks for missing values, impossible values, and the presence 
#' of demographic columns in a generated dataset. It is designed for use in 
#' teaching or simulation settings where generated Likert-style item data and 
#' demographics are combined.
#'
#' @param dat A data frame containing the generated dataset.
#' @param impossible_value An integer value considered impossible for the 
#'   simulated data (default: 99).
#'
#' @return Prints messages to the console about detected issues and returns 
#'   \code{TRUE} invisibly if the checks pass without problems.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = 1:5,
#'   age = c(25, 30, 40, NA, 35),
#'   gender = c("male", "female", "male", "female", "female"),
#'   X1 = c(1, 2, 3, 4, 99),
#'   X2 = c(2, 3, 4, 5, 1)
#' )
#' check_generated_data(df, impossible_value = 99)
#' }
#'
#' @export
#' @importFrom dplyr across select where
#' @importFrom stats complete.cases
check_generated_data <- function(dat){
  dat_sumcores <- dat %>%
    select(-id, -condition, -gender, -age) |>
    add_sum_scores_by_scale() |> # returns X1_sum, X2_sum, X3_sum
    select(ends_with("_sum"))
  
  dat_sumcores$condition <- dat$condition
  dat_sumcores$id <- dat$id
  
  cohens_d_function <- function(x, g) {
    m <- tapply(x, g, mean)
    s <- tapply(x, g, sd)
    n <- table(g)
    sp <- sqrt(((n[1]-1)*s[1]^2 + (n[2]-1)*s[2]^2) / (sum(n)-2))
    unname((m[2] - m[1]) / sp)  # treatment - control
  }
  
  cohens_d <- c(
    d_X1_sum = cohens_d_function(dat_sumcores$X1_sum, dat_sumcores$condition),
    d_X2_sum = cohens_d_function(dat_sumcores$X2_sum, dat_sumcores$condition),
    d_X3_sum = cohens_d_function(dat_sumcores$X3_sum, dat_sumcores$condition)
  ) |>
    round(2)
  
  r <- dat_sumcores %>%
    select(-id, -condition) |>
    cor() |>
    round(2)
  
  item_histograms <- dat |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "item",
                 values_to = "score") |>
    ggplot(aes(score)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(item ~ condition)
  
  return(list(cohens_d = cohens_d,
              r = r,
              item_histograms = item_histograms))
}