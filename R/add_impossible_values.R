add_impossible_values <- function(dat, proportion_impossible = 0.03, replacement_value = 99L) {
  dat %>%
    mutate(across(starts_with("X"), ~ replace(.x, runif(length(.x)) < proportion_impossible, replacement_value)))
}