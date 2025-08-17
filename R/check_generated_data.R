check_generated_data <- function(dat){
  dat_sumcores <- dat %>%
    select(-id, -condition, -gender, -age) |>
    sum_scores_by_scale()  # returns X1_sum, X2_sum, X3_sum
  
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