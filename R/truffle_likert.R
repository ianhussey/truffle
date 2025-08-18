#' Generate Likert-Scale Data for Two Conditions (with latent mean shifts)
#'
#' Simulate item-level Likert data for two groups (e.g., control vs. treatment)
#' by (i) specifying a multi-factor latent variable model, (ii) shifting latent
#' means by a specified Cohen's \emph{d} in the treatment group, and
#' (iii) discretizing continuous indicators to Likert categories using shared
#' cutpoints across groups (to preserve mean differences).
#'
#' @param study_design Character scalar. Currently only \code{"factorial_between2"}
#'   is supported (two independent groups). Default \code{"factorial_between2"}.
#' @param n_per_condition Integer. Number of participants per group (default \code{100}).
#' @param factors Character vector (length \eqn{K}). Latent factor names. If \code{NULL},
#'   defaults to \code{X1_latent}, \code{X2_latent}, \dots.
#' @param prefixes Character vector (length \eqn{K}). Item name prefixes per factor.
#'   If \code{NULL}, defaults to \code{X1_item}, \code{X2_item}, \dots.
#' @param alpha Numeric scalar or length-\eqn{K} vector. Target Cronbach's alpha
#'   for each factor (default \code{0.70}). Scalars are recycled.
#' @param n_items Integer vector (length \eqn{K}). Number of items per factor (each \eqn{\ge} 2).
#' @param n_levels Integer. Number of Likert response categories (default \code{7}).
#' @param r_among_outcomes Either a single latent correlation in \eqn{(-1,1)} applied to
#'   all pairs, or a \eqn{K \times K} latent correlation matrix (symmetric, unit diagonal).
#' @param approx_d_between_groups Numeric. Either a single Cohen's \emph{d} applied to
#'   \emph{all} latents, or a length-\eqn{K} vector (optionally \emph{named} by \code{factors})
#'   giving per-latent \emph{d} values. Scalars are recycled; named vectors are
#'   matched to \code{factors}.
#' @param condition_names Character length-2 vector giving group labels
#'   (default \code{c("control","treatment")}).
#' @param seed Optional integer seed for reproducibility.
#' @param lv_var Numeric scalar or length-\eqn{K} vector of latent variances (default \code{1}).
#' @param x_var Numeric scalar or length-\eqn{K} vector of item variances (default \code{1}).
#' @param return_continuous Logical. If \code{TRUE}, return a list including the
#'   continuous simulated data and model strings in addition to the Likert data.
#'
#' @details
#' The base measurement model and latent covariance structure are produced by
#' the internal helper \code{.make_lavaan_kfactor_corr()}, which converts
#' alphas into tau-equivalent loadings and sets residual variances so that
#' item variances equal \code{x_var}. Latent means are fixed at 0 in the control
#' group and shifted by \code{approx_d_between_groups} in the treatment group
#' (on the latent scale; when \code{lv_var = 1}, the shift equals Cohen's \emph{d}).
#'
#' Continuous indicators are simulated via \code{lavaan::simulateData()} and
#' discretized to Likert scores using shared cutpoints (fixed reference
#' \eqn{\mu=0}, \eqn{\sigma=1}) across groups. This avoids washing out the
#' treatment mean shift that would occur if cutpoints were fit per group/column.
#'
#' @return
#' If \code{return_continuous = FALSE} (default): a data frame with columns
#' \itemize{
#'   \item \code{id} — participant ID (integer sequence)
#'   \item \code{condition} — group label (\code{condition_names})
#'   \item item responses — Likert-scale items named \code{<prefix><index>}
#' }
#' If \code{return_continuous = TRUE}: a list with elements
#' \itemize{
#'   \item \code{dat_lik} — Likert data frame as above
#'   \item \code{dat_ctrl_cont}, \code{dat_treat_cont} — continuous data per group
#'   \item \code{item_cols} — character vector of item column names
#'   \item \code{models} — list with \code{base}, \code{ctrl}, \code{treat} lavaan syntax
#'   \item \code{d_per_factor} — the per-latent \emph{d} vector actually used
#' }
#'
#' @examples
#' \dontrun{
#' # Three-factor example with a common d applied to all latents
#' dat <- truffle_likert(
#'   study_design = "factorial_between2",
#'   n_items     = c(5, 6, 7),
#'   alpha       = c(.70, .75, .80),
#'   factors     = c("X1_latent","X2_latent","X3_latent"),
#'   prefixes    = c("X1_item","X2_item","X3_item"),
#'   r_among_outcomes = 0.3,
#'   approx_d_between_groups = 0.5,
#'   n_per_condition = 300,
#'   n_levels    = 5,
#'   seed = 123
#' )
#'
#' # Per-latent d's (named vector, order-independent)
#' dat2 <- truffle_likert(
#'   n_items   = c(5, 6, 7),
#'   alpha     = .75,
#'   approx_d_between_groups = c(X2_latent = 0.2, X1_latent = 0.5, X3_latent = 0.1),
#'   seed = 42
#' )
#' }
#'
#' @seealso \code{\link[lavaan]{simulateData}}
#' @export
#' @importFrom lavaan simulateData
#' @importFrom dplyr bind_rows mutate relocate row_number
truffle_likert <- function(
    study_design = "factorial_between2", # only design possible right now
    n_per_condition = 100,
    factors     = NULL,
    prefixes    = NULL,
    alpha       = 0.70,
    n_items,
    n_levels    = 7,
    r_among_outcomes = 0.30, # <- scalar or matrix
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
  
  if (!study_design %in% c("factorial_between2")){
    stop("study_design must be one of c('factorial_between2')")
  }
  
  # if factorial design between two groups study:
  # (only one possible for the moment)
  if (study_design == "factorial_between2"){
    
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
    dat_ctrl_lik <- .continuous_to_likert_by_condition(
      dat_ctrl_cont[item_cols],
      n_levels = n_levels, ordered = FALSE,
      method = "fixed", mu_ref = 0, sd_ref = 1
    )
    dat_treat_lik <- .continuous_to_likert_by_condition(
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
}
