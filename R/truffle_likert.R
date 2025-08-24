#' Generate Likert-Scale Data for Two-Group or Single-Group (Cross-Sectional) Designs
#'
#' Simulate item-level Likert data under a multi-factor latent variable model.
#' Supports:
#'   (a) two independent groups with latent mean shifts ("factorial_between2"), or
#'   (b) a single-group cross-sectional design with specified latent correlations
#'       ("crosssectional").
#'
#' @param study_design Character scalar. One of \code{"factorial_between2"} (default)
#'   or \code{"crosssectional"}.
#' @param n_per_condition Integer. For \code{"factorial_between2"}: participants per group.
#'   For \code{"crosssectional"}: total N.
#' @param factors Character vector (length K). Latent factor names. If \code{NULL},
#'   defaults to \code{X1_latent}, \code{X2_latent}, ...
#' @param prefixes Character vector (length K). Item name prefixes per factor.
#'   If \code{NULL}, defaults to \code{X1_item}, \code{X2_item}, ...
#' @param alpha Numeric scalar or length-K vector. Target Cronbach's alpha per factor (default 0.70).
#' @param n_items Integer vector (length K). Number of items per factor (each >= 2).
#' @param n_levels Integer vector (length K). Number of Likert categories (default 7). Either a scalar 
#'   K applied to all latents or a length-K vector.
#' @param r_among_outcomes Either a single latent correlation in (-1,1) applied to all
#'   pairs, or a K x K latent correlation matrix (symmetric, unit diagonal).
#' @param approx_d_between_groups Numeric. Only used for \code{"factorial_between2"}.
#'   Either a scalar \(d\) applied to all latents or a length-K vector (optionally named).
#' @param condition_names Character length-2 vector of group labels (default c("control","treatment")).
#'   Ignored for \code{"crosssectional"}.
#' @param seed Optional integer seed.
#' @param lv_var Numeric scalar or length-K vector of latent variances (default 1).
#' @param x_var Numeric scalar or length-K vector of item variances (default 1).
#' @param return_continuous Logical. If TRUE, return a list including continuous data and model strings.
#'
#' @return
#' If \code{return_continuous = FALSE}:
#'   * \code{"factorial_between2"}: data frame with \code{id}, \code{condition}, and item columns.
#'   * \code{"crosssectional"}: data frame with \code{id} and item columns.
#'
#' If \code{return_continuous = TRUE}:
#'   * \code{"factorial_between2"}: list with \code{dat_lik}, \code{dat_ctrl_cont}, \code{dat_treat_cont},
#'     \code{item_cols}, \code{models} (base/ctrl/treat), and \code{d_per_factor}.
#'   * \code{"crosssectional"}: list with \code{dat_lik}, \code{dat_cont}, \code{item_cols},
#'     and \code{models} (base and means=0).
#'
#' @examples
#' \dontrun{
#' # Cross-sectional, three factors, scalar r among latents
#' dat_cs <- truffle_likert(
#'   study_design = "crosssectional",
#'   n_items   = c(5, 6, 7),
#'   alpha     = c(.70, .75, .80),
#'   r_among_outcomes = 0.3,
#'   n_per_condition = 500, # total N in cross-sectional mode
#'   n_levels  = c(10, 7, 7),
#'   seed = 123
#' )
#'
#' # Cross-sectional with a custom latent correlation matrix
#' R <- matrix(c(
#'   1,   .4, .2,
#'   .4,   1, .3,
#'   .2,  .3, 1
#' ), 3, 3, byrow = TRUE)
#' 
#' dat_cs2 <- truffle_likert(
#'   study_design = "crosssectional",
#'   n_items   = c(4, 4, 4),
#'   alpha     = .75,
#'   r_among_outcomes = R,
#'   n_per_condition = 400,
#'   seed = 42
#' )
#' 
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
#' @export
#' @importFrom lavaan simulateData
#' @importFrom dplyr bind_rows mutate relocate row_number
truffle_likert <- function(
    study_design = "factorial_between2",
    n_per_condition = 100,
    factors     = NULL,
    prefixes    = NULL,
    alpha       = 0.70,
    n_items,
    n_levels    = 7,             # now scalar OR length-K
    r_among_outcomes = 0.30,
    approx_d_between_groups = 0.50,
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
  
  if (!study_design %in% c("factorial_between2", "crosssectional")){
    stop("study_design must be one of c('factorial_between2','crosssectional').")
  }
  
  # --- validate/prepare n_levels (scalar or length-K) ---
  if (!is.numeric(n_levels) || any(!is.finite(n_levels)) || any(n_levels < 2)) {
    stop("`n_levels` must be integer-like >= 2 (scalar or length-K).")
  }
  if (length(n_levels) == 1L) {
    n_levels_vec <- rep(as.integer(n_levels), K)
  } else if (length(n_levels) == K) {
    n_levels_vec <- as.integer(n_levels)
  } else {
    stop("When a vector, `n_levels` must have length K = ", K, ".")
  }
  
  # --- helpers ---
  .regex_escape <- function(x) gsub("([][{}()+*^$|\\.?*\\\\])", "\\\\\\1", x)
  escaped_prefixes <- vapply(prefixes, .regex_escape, character(1))
  pref_regex <- paste0("^(", paste0(escaped_prefixes, collapse = "|"), ")\\d+$")
  
  # split detected item columns into blocks by prefix (preserving factor order)
  .split_cols_by_prefix <- function(cols, prefixes) {
    out <- vector("list", length(prefixes))
    names(out) <- prefixes
    for (i in seq_along(prefixes)) {
      rx <- paste0("^", .regex_escape(prefixes[i]), "\\d+$")
      out[[i]] <- grep(rx, cols, value = TRUE)
    }
    out
  }
  
  # Discretize a continuous data.frame by blocks with per-factor n_levels
  .discretize_by_blocks <- function(df_cont, blocks, n_levels_vec) {
    stopifnot(length(blocks) == length(n_levels_vec))
    pieces <- vector("list", length(blocks))
    for (i in seq_along(blocks)) {
      blk <- blocks[[i]]
      if (length(blk) == 0L) next
      pieces[[i]] <- .continuous_to_likert_by_condition(
        df_cont[blk],
        n_levels = n_levels_vec[i],
        ordered  = FALSE,
        method   = "fixed",
        mu_ref   = 0,
        sd_ref   = 1
      )
    }
    # bind back in factor order; columns are disjoint by construction
    keep <- !vapply(pieces, is.null, logical(1))
    dplyr::bind_cols(pieces[keep])
  }
  
  # --- measurement + latent covariance model ---
  mod <- .make_lavaan_kfactor_corr(
    n_items     = n_items,
    alpha       = alpha,
    lv_var      = lv_var,
    x_var       = x_var,
    factors     = factors,
    prefixes    = prefixes,
    corr_latent = r_among_outcomes
  )
  
  # -------------------------------
  # Case 1: Two-group factorial
  # -------------------------------
  if (study_design == "factorial_between2") {
    
    # d handling
    if (!is.numeric(approx_d_between_groups) || any(!is.finite(approx_d_between_groups))) {
      stop("`approx_d_between_groups` must be numeric and finite (scalar or length-K).")
    }
    if (length(approx_d_between_groups) == 1L) {
      d_vec <- rep(approx_d_between_groups, K)
      names(d_vec) <- factors
    } else {
      d_vec <- approx_d_between_groups
      if (!is.null(names(d_vec))) {
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
    
    # simulate continuous
    dat_ctrl_cont  <- lavaan::simulateData(mod_ctrl,  sample.nobs = n_per_condition)
    dat_treat_cont <- lavaan::simulateData(mod_treat, sample.nobs = n_per_condition)
    dat_ctrl_cont$condition  <- "control"
    dat_treat_cont$condition <- "treatment"
    
    # item columns
    item_cols_ctrl  <- grep(pref_regex, names(dat_ctrl_cont),  value = TRUE)
    item_cols_treat <- grep(pref_regex, names(dat_treat_cont), value = TRUE)
    if (!identical(sort(item_cols_ctrl), sort(item_cols_treat))) {
      stop("Item columns differ across conditions; check prefixes or model.")
    }
    # blocks in the canonical factor/prefix order
    blocks <- .split_cols_by_prefix(item_cols_ctrl, prefixes)
    
    # discretize by blocks with per-factor levels
    dat_ctrl_lik  <- .discretize_by_blocks(dat_ctrl_cont,  blocks, n_levels_vec)
    dat_treat_lik <- .discretize_by_blocks(dat_treat_cont, blocks, n_levels_vec)
    
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
        item_cols      = unlist(blocks, use.names = FALSE),
        models         = list(base = mod, ctrl = mod_ctrl, treat = mod_treat),
        d_per_factor   = d_vec,
        n_levels_per_factor = setNames(n_levels_vec, factors)
      ))
    } else {
      return(dat_lik)
    }
  }
  
  # -------------------------------
  # Case 2: Single-group cross-sectional
  # -------------------------------
  if (study_design == "crosssectional") {
    if (!identical(condition_names, c("control","treatment"))) {
      warning("`condition_names` is ignored for 'crosssectional'.")
    }
    means_zero <- paste0(factors, " ~ 0*1", collapse = "\n")
    mod_cs <- paste(mod, means_zero, sep = "\n")
    
    dat_cont <- lavaan::simulateData(mod_cs, sample.nobs = n_per_condition)
    
    item_cols <- grep(pref_regex, names(dat_cont), value = TRUE)
    blocks <- .split_cols_by_prefix(item_cols, prefixes)
    
    dat_lik <- .discretize_by_blocks(dat_cont, blocks, n_levels_vec)
    
    dat_lik$id <- seq_len(nrow(dat_lik))
    dat_lik <- dplyr::relocate(dat_lik, id)
    
    if (return_continuous) {
      return(list(
        dat_lik   = dat_lik,
        dat_cont  = dat_cont,
        item_cols = unlist(blocks, use.names = FALSE),
        models    = list(base = mod, means0 = mod_cs),
        n_levels_per_factor = setNames(n_levels_vec, factors)
      ))
    } else {
      return(dat_lik)
    }
  }
}