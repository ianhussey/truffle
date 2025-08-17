generate_data_likert_two_conditions <- function(
    n_per_condition = 100,
    factors     = NULL,        # names of the K latents (defaults F1..FK if NULL)
    prefixes    = NULL,        # item name prefixes (defaults X1_item, X2_item, ...)
    alpha       = 0.70,        # scalar or length-K
    n_items,                   # integer vector length K (items per factor)
    n_levels    = 7,           # Likert categories
    r_among_outcomes = 0.30,        # scalar or KxK correlation matrix
    approx_d_between_groups    = 0.50,        # scalar (applied to ALL latents) or length-K vector
    condition_names = c("control", "treatment"),
    seed        = NULL,
    lv_var      = 1,           # scalar or length-K
    x_var       = 1,           # scalar or length-K
    return_continuous = FALSE  # also return continuous data/models if TRUE
) {
  if (!is.null(seed)) set.seed(seed)
  
  K <- length(n_items)
  stopifnot(K >= 1, all(n_items >= 2))
  
  # default factor names / prefixes if not supplied
  if (is.null(factors))  factors  <- paste0("X", seq_len(K), "_latent")
  if (is.null(prefixes)) prefixes <- paste0("X", seq_len(K), "_item")
  
  # build structural (covariance) part and measurement with your helper
  mod <- make_lavaan_kfactor_corr(
    n_items     = n_items,
    alpha       = alpha,
    lv_var      = lv_var,
    x_var       = x_var,
    factors     = factors,
    prefixes    = prefixes,
    corr_latent = r_among_outcomes
  )
  
  # approx_d_between_groups can be scalar or length-K; recycle if needed
  if (length(approx_d_between_groups) == 1L) approx_d_between_groups <- rep(approx_d_between_groups, K)
  stopifnot(length(approx_d_between_groups) == K)
  
  # latent mean structures for control (all 0) and treatment (shifted)
  means_ctrl  <- paste0(factors, " ~ 0*1", collapse = "\n")
  means_treat <- paste0(factors, " ~ ", approx_d_between_groups, "*1", collapse = "\n")
  
  mod_ctrl  <- paste(mod, means_ctrl,  sep = "\n")
  mod_treat <- paste(mod, means_treat, sep = "\n")
  
  # simulate continuous data
  dat_ctrl_cont  <- lavaan::simulateData(mod_ctrl,  sample.nobs = n_per_condition)
  dat_treat_cont <- lavaan::simulateData(mod_treat, sample.nobs = n_per_condition)
  dat_ctrl_cont$condition  <- "control"
  dat_treat_cont$condition <- "treatment"
  
  # item column regex from prefixes, e.g., ^(X1_item|X2_item|...)\d+$
  pref_regex <- paste0("^(", paste0(prefixes, collapse = "|"), ")\\d+$")
  item_cols_ctrl  <- grep(pref_regex, names(dat_ctrl_cont),  value = TRUE)
  item_cols_treat <- grep(pref_regex, names(dat_treat_cont), value = TRUE)
  if (!identical(sort(item_cols_ctrl), sort(item_cols_treat))) {
    stop("Item columns differ across conditions; check prefixes or model.")
  }
  item_cols <- item_cols_ctrl
  
  # discretize using a FIXED reference (mu_ref=0, sd_ref=1) with shared cutpoints
  dat_ctrl_lik <- continuous_to_likert_by_condition(
    dat_ctrl_cont[item_cols],
    n_levels = n_levels,
    ordered  = FALSE,
    method   = "fixed",
    mu_ref   = 0,
    sd_ref   = 1
  )
  dat_treat_lik <- continuous_to_likert_by_condition(
    dat_treat_cont[item_cols],
    n_levels = n_levels,
    ordered  = FALSE,
    method   = "fixed",
    mu_ref   = 0,
    sd_ref   = 1
  )
  
  # add condition and bind
  dat_ctrl_lik$condition  <- condition_names[1]
  dat_treat_lik$condition <- condition_names[2]
  dat_lik <- dplyr::bind_rows(dat_ctrl_lik, dat_treat_lik) |>
    mutate(id = row_number()) |>
    dplyr::relocate(id, .before = 1) |>
    dplyr::relocate(condition, .after = id)
  
  if (return_continuous) {
    return(list(
      dat_lik        = dat_lik,
      dat_ctrl_cont  = dat_ctrl_cont,
      dat_treat_cont = dat_treat_cont,
      item_cols      = item_cols,
      models         = list(base = mod, ctrl = mod_ctrl, treat = mod_treat)
    ))
  } else {
    return(dat_lik)
  }
}