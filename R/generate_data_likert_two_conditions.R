#' Generate Likert-Scale Data for Two Experimental Conditions
#'
#' This function generates item-level Likert-scale data for two groups 
#' (control and treatment), with a specified mean shift on the latent variables. 
#' Data are simulated from a latent variable model using \code{lavaan::simulateData} 
#' and then discretized into Likert categories with shared cutpoints.
#'
#' @param n_items Integer vector giving the number of items per factor.
#' @param alpha Numeric vector of target Cronbach's alpha reliabilities 
#'   for each factor.
#' @param lv_var Numeric; latent variance (default = 1).
#' @param x_var Numeric; item variance (default = 1).
#' @param factors Character vector of latent factor names.
#' @param prefixes Character vector of item name prefixes.
#' @param corr_latent Either a single numeric correlation applied across all 
#'   latent pairs, or a correlation matrix.
#' @param approx_d_between_groups Numeric. Either a single Cohen’s d applied
#' to **all** latents, or a length-K vector (optionally named by `factors`)
#' to set different d-values per latent.
#' @param n_per_group Integer; number of participants per group (default = 1000).
#' @param n_levels Integer; number of Likert response categories (default = 7).
#' @param ordered Logical; whether the Likert responses are returned as 
#'   ordered factors (default = TRUE).
#'
#' @details
#' The function builds a correlated latent factor model using 
#' \code{.make_lavaan_kfactor_corr()}. Latent means are set to zero for the 
#' control group and shifted by \code{d_target} for the treatment group.  
#' Continuous data are then simulated via \code{lavaan::simulateData}, 
#' discretized into Likert scales with shared cutpoints across groups, 
#' and returned as a single combined dataset.
#'
#' @return A data frame containing Likert-scale item responses for both 
#' groups, with columns:
#' \itemize{
#'   \item \code{id} — participant ID
#'   \item \code{group} — experimental group ("control" or "treatment")
#'   \item item responses (Likert-scale)
#' }
#'
#' @examples
#' \dontrun{
#' dat <- generate_data_likert_two_conditions(
#'   n_items     = c(5, 6, 7),
#'   alpha       = c(.70, .75, .80),
#'   factors     = c("X1_latent","X2_latent","X3_latent"),
#'   prefixes    = c("X1_item","X2_item","X3_item"),
#'   corr_latent = 0.3,
#'   d_target    = 0.5,
#'   n_per_group = 500,
#'   n_levels    = 5
#' )
#' head(dat)
#' }
#'
#' @export
#' @importFrom lavaan simulateData
#' @importFrom dplyr bind_rows mutate relocate
#' @importFrom stats rnorm
#' @importFrom latent2likert discretize_density
generate_data_likert_two_conditions <- function(
    n_per_condition = 100,
    factors     = NULL,
    prefixes    = NULL,
    alpha       = 0.70,
    n_items,
    n_levels    = 7,
    r_among_outcomes = 0.30,
    approx_d_between_groups = 0.50,   # <- scalar or length-K
    condition_names = c("control", "treatment"),
    seed        = NULL,
    lv_var      = 1,
    x_var       = 1,
    return_continuous = FALSE
) {
  if (!is.null(seed)) set.seed(seed)
  
  K <- length(n_items)
  stopifnot(K >= 1, all(n_items >= 2))
  
  if (is.null(factors))  factors  <- paste0("X", seq_len(K), "_latent")
  if (is.null(prefixes)) prefixes <- paste0("X", seq_len(K), "_item")
  
  mod <- .make_lavaan_kfactor_corr(
    n_items     = n_items,
    alpha       = alpha,
    lv_var      = lv_var,
    x_var       = x_var,
    factors     = factors,
    prefixes    = prefixes,
    corr_latent = r_among_outcomes
  )
  
  ## ---- d-target handling: scalar or length-K (optionally named) ----
  if (!is.numeric(approx_d_between_groups) || any(!is.finite(approx_d_between_groups))) {
    stop("`approx_d_between_groups` must be numeric and finite (scalar or length-K).")
  }
  if (length(approx_d_between_groups) == 1L) {
    d_vec <- rep(approx_d_between_groups, K)
    names(d_vec) <- factors
  } else {
    d_vec <- approx_d_between_groups
    if (!is.null(names(d_vec))) {
      # reorder to match `factors`
      if (!all(factors %in% names(d_vec))) {
        stop("Named `approx_d_between_groups` must include all factor names: ",
             paste(factors, collapse = ", "))
      }
      d_vec <- d_vec[factors]
    } else if (length(d_vec) != K) {
      stop("If not scalar and unnamed, `approx_d_between_groups` must have length K = ", K, ".")
    }
  }
  
  # latent means: control = 0; treatment = per-factor d
  means_ctrl  <- paste0(factors, " ~ 0*1", collapse = "\n")
  means_treat <- paste0(factors, " ~ ", d_vec, "*1", collapse = "\n")
  
  mod_ctrl  <- paste(mod, means_ctrl,  sep = "\n")
  mod_treat <- paste(mod, means_treat, sep = "\n")
  
  # simulate continuous data
  dat_ctrl_cont  <- lavaan::simulateData(mod_ctrl,  sample.nobs = n_per_condition)
  dat_treat_cont <- lavaan::simulateData(mod_treat, sample.nobs = n_per_condition)
  dat_ctrl_cont$condition  <- "control"
  dat_treat_cont$condition <- "treatment"
  
  # item columns
  pref_regex <- paste0("^(", paste0(prefixes, collapse = "|"), ")\\d+$")
  item_cols_ctrl  <- grep(pref_regex, names(dat_ctrl_cont),  value = TRUE)
  item_cols_treat <- grep(pref_regex, names(dat_treat_cont), value = TRUE)
  if (!identical(sort(item_cols_ctrl), sort(item_cols_treat))) {
    stop("Item columns differ across conditions; check prefixes or model.")
  }
  item_cols <- item_cols_ctrl
  
  # discretize with shared cutpoints
  dat_ctrl_lik <- continuous_to_likert_by_condition(
    dat_ctrl_cont[item_cols],
    n_levels = n_levels, ordered = FALSE,
    method = "fixed", mu_ref = 0, sd_ref = 1
  )
  dat_treat_lik <- continuous_to_likert_by_condition(
    dat_treat_cont[item_cols],
    n_levels = n_levels, ordered = FALSE,
    method = "fixed", mu_ref = 0, sd_ref = 1
  )
  
  dat_ctrl_lik$condition  <- condition_names[1]
  dat_treat_lik$condition <- condition_names[2]
  
  dat_lik <- dplyr::bind_rows(dat_ctrl_lik, dat_treat_lik) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::relocate(id, .before = 1) |>
    dplyr::relocate(condition, .after = id)
  
  if (return_continuous) {
    return(list(
      dat_lik        = dat_lik,
      dat_ctrl_cont  = dat_ctrl_cont,
      dat_treat_cont = dat_treat_cont,
      item_cols      = item_cols,
      models         = list(base = mod, ctrl = mod_ctrl, treat = mod_treat),
      d_per_factor   = d_vec
    ))
  } else {
    return(dat_lik)
  }
}
