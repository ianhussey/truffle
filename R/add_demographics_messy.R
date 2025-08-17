add_demographics_messy <- function(
    dat,
    p_messy_age      = 0.05,                  # age column: prob of messy entries
    gender_probs     = c(male = .30, female = .70),
    p_age_in_gender  = 0.05,                  # gender column: prob replaced by a number age
    p_fmt_misspell   = 0.10                   # among correct genders, prob of case/misspell tweaks
) {
  stopifnot(is.data.frame(dat))
  n <- nrow(dat)
  
  # ----- age (95% numeric 18:45; 5% spelled or 'male'/'female') -----
  spelled <- c(
    "eighteen","nineteen",
    "twenty","twenty one","twenty two","twenty three","twenty four","twenty five",
    "twenty six","twenty seven","twenty eight","twenty nine",
    "thirty","thirty one","thirty two","thirty three","thirty four","thirty five",
    "thirty six","thirty seven","thirty eight","thirty nine",
    "forty","forty one","forty two","forty three","forty four","forty five"
  )
  messy_pool_age <- c(spelled, "male", "female")
  
  is_messy_age <- runif(n) < p_messy_age
  age_chr <- character(n)
  age_chr[!is_messy_age] <- as.character(sample(18:45, sum(!is_messy_age), replace = TRUE))
  age_chr[ is_messy_age] <- sample(messy_pool_age, sum(is_messy_age),  replace = TRUE)
  
  # ----- gender (base correct genders by prob) -----
  g_names <- names(gender_probs)
  g_probs <- as.numeric(gender_probs)
  gender_chr <- sample(g_names, size = n, replace = TRUE, prob = g_probs)
  
  # (A) 5%: replace gender with a numeric age
  idx_age_in_gender <- runif(n) < p_age_in_gender
  gender_chr[idx_age_in_gender] <- as.character(sample(18:45, sum(idx_age_in_gender), replace = TRUE))
  
  # (B) among the remaining “correct” genders, 10%: case/misspell tweaks
  idx_correct <- !idx_age_in_gender
  idx_tweak   <- idx_correct & (runif(n) < p_fmt_misspell)
  
  # tweak function: case variants + misspellings
  tweak_gender <- function(g) {
    # pools: mostly case variants, some misspellings
    if (g == "male") {
      pool <- c("Male","MALE","male","MalE",           # case variants / “title”
                "mal","malee","ma le","mle")           # misspellings
    } else {
      pool <- c("Female","FEMALE","female","FeMale",   # case variants / “title”
                "femail","femlae","femle","fem ale")   # misspellings
    }
    sample(pool, 1)
  }
  
  if (any(idx_tweak)) {
    gender_chr[idx_tweak] <- vapply(gender_chr[idx_tweak], tweak_gender, character(1))
  }
  
  # ----- assemble & order -----
  dat |>
    dplyr::mutate(age = age_chr,
                  gender = gender_chr) |>
    dplyr::relocate(id, .before = 1) |>
    dplyr::relocate(age, .after = id) |>
    dplyr::relocate(gender, .after = age)
}