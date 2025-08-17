#' Build a K-Factor Correlated Latent Variable Model for lavaan
#'
#' Construct a lavaan model string for simulating data from K latent factors,
#' each with a specified number of items and target reliability (Cronbach's alpha).
#' Works for K = 1 (no latent correlations) and K >= 2.
#'
#' @param n_items Integer vector giving the number of items per factor (length K).
#' @param alpha Numeric scalar or length-K vector of target alphas.
#' @param lv_var Numeric scalar or length-K latent variances (default 1).
#' @param x_var Numeric scalar or length-K item variances (default 1).
#' @param factors Optional character vector (length K) of latent names (default F1, F2, ...).
#' @param prefixes Optional character vector (length K) of item name prefixes (default X1_item, ...).
#' @param corr_latent Either a single correlation in (-1,1) (applied to all pairs),
#'   or a K x K correlation matrix. May be \code{NULL} when K = 1.
#' @param round_loading Digits to round loadings in output.
#' @param signif_resid  Significant digits for residual variances.
#' @param signif_cov    Significant digits for latent covariances.
#'
#' @return A character string of lavaan syntax.
#' @keywords internal
.make_lavaan_kfactor_corr <- function(
    n_items,
    alpha    = 0.70,
    lv_var   = 1,
    x_var    = 1,
    factors  = NULL,
    prefixes = NULL,
    corr_latent = 0.30,  # ignored when K = 1; may be NULL when K = 1
    round_loading = 3,
    signif_resid  = 6,
    signif_cov    = 6
) {
  K <- length(n_items)
  stopifnot(K >= 1, all(n_items >= 2))
  
  # recycle scalars to length-K
  as_lenK <- function(x) if (length(x) == 1) rep(x, K) else x
  alpha  <- as_lenK(alpha)
  lv_var <- as_lenK(lv_var)
  x_var  <- as_lenK(x_var)
  if (any(c(lv_var, x_var) <= 0)) stop("All variances must be > 0.")
  if (length(alpha) != K || length(lv_var) != K || length(x_var) != K)
    stop("alpha, lv_var, x_var must be scalar or length K.")
  
  if (is.null(factors))  factors  <- paste0("F", seq_len(K))
  if (is.null(prefixes)) prefixes <- paste0("X", seq_len(K), "_item")
  if (length(factors) != K || length(prefixes) != K)
    stop("factors and prefixes (if provided) must be length K.")
  
  # --- helper to build a single factor block from alpha ---
  .one_block <- function(n_i, alpha_i, lv_var_i, x_var_i, factor_i, prefix_i) {
    # avg inter-item correlation under tau-equivalence
    rho     <- alpha_i / (n_i - (n_i - 1) * alpha_i)
    loading <- sqrt(rho)
    resid   <- x_var_i - loading^2 * lv_var_i
    if (resid <= 0)
      stop("Non-positive residual variance for ", factor_i,
           ". Adjust alpha / n_items / lv_var / x_var.")
    
    inds <- paste0(prefix_i, seq_len(n_i))
    meas <- paste0(factor_i, " =~ ",
                   paste(paste0(round(loading, round_loading), "*", inds), collapse = " + "))
    lvar <- paste0(factor_i, " ~~ ", lv_var_i, "*", factor_i)
    rvec <- paste0(inds, " ~~ ", signif(resid, signif_resid), "*", inds)
    
    paste(c(meas, lvar, rvec), collapse = "\n")
  }
  
  # --- build all factor blocks ---
  blocks <- vapply(seq_len(K), function(k) {
    .one_block(n_items[k], alpha[k], lv_var[k], x_var[k], factors[k], prefixes[k])
  }, character(1))
  
  # --- latent links ---
  links <- character(0)
  if (K >= 2) {
    if (is.null(corr_latent)) {
      stop("corr_latent must be provided (scalar or K×K matrix) when K >= 2.")
    }
    
    # Build latent correlation matrix R
    if (is.matrix(corr_latent)) {
      if (!all(dim(corr_latent) == c(K, K)))
        stop("corr_latent matrix must be K x K.")
      if (!isTRUE(all.equal(corr_latent, t(corr_latent))))
        stop("corr_latent must be symmetric.")
      if (!all(diag(corr_latent) == 1))
        stop("Diagonal of corr_latent must be 1.")
      if (any(abs(corr_latent) > 1))
        stop("Correlations must be in [-1, 1].")
      R <- corr_latent
    } else {
      r <- as.numeric(corr_latent)
      if (length(r) != 1 || !is.finite(r) || abs(r) >= 1)
        stop("Scalar corr_latent must be a finite value strictly between -1 and 1.")
      if (r <= -1/(K - 1))
        stop(sprintf("For equicorrelation, r must be > -1/(K-1) = %.3f.", -1/(K - 1)))
      R <- matrix(r, K, K); diag(R) <- 1
    }
    
    # Convert correlations to covariances
    sd_lat <- sqrt(lv_var)
    Dhalf  <- diag(sd_lat, nrow = K)
    Psi    <- Dhalf %*% R %*% Dhalf
    
    # build link lines i<j
    for (i in 1:(K - 1)) for (j in (i + 1):K) {
      links <- c(links, paste0(factors[i], " ~~ ",
                               signif(Psi[i, j], signif_cov), "*", factors[j]))
    }
  }
  # if K == 1: links stays empty, corr_latent (including NULL) is ignored
  
  paste(c(blocks, links), collapse = "\n")
}
