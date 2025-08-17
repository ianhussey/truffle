make_lavaan_kfactor_corr <- function(
    n_items,                  # integer vector length K
    alpha    = 0.70,          # scalar or length-K
    lv_var   = 1,             # scalar or length-K (latent variances)
    x_var    = 1,             # scalar or length-K (indicator variances)
    factors  = NULL,          # optional names length-K (defaults: F1, F2, ...)
    prefixes = NULL,          # optional prefixes length-K (defaults: X1_item, ...)
    corr_latent = 0.30,       # either scalar r in (-1,1) OR KxK correlation matrix
    round_loading = 3,
    signif_resid  = 6,
    signif_cov    = 6
) {
  K <- length(n_items)
  stopifnot(K >= 2, all(n_items >= 2))
  
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
  
  # --- Build latent correlation matrix R ---
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
    # scalar equicorrelation, check feasibility r > -1/(K-1)
    r <- as.numeric(corr_latent)
    if (length(r) != 1 || !is.finite(r) || abs(r) >= 1)
      stop("Scalar corr_latent must be a finite value strictly between -1 and 1.")
    if (r <= -1/(K - 1))
      stop(sprintf("For equicorrelation, r must be > -1/(K-1) = %.3f.", -1/(K - 1)))
    R <- matrix(r, K, K); diag(R) <- 1
  }
  
  # --- Convert correlations to covariances for lavaan ---
  sd_lat <- sqrt(lv_var)
  Dhalf  <- diag(sd_lat, nrow = K)
  Psi    <- Dhalf %*% R %*% Dhalf  # latent covariance matrix
  
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
  
  # --- link latents using Psi (covariances) ---
  links <- character()
  if (K >= 2) {
    for (i in 1:(K - 1)) for (j in (i + 1):K) {
      links <- c(links, paste0(factors[i], " ~~ ",
                               signif(Psi[i, j], signif_cov), "*", factors[j]))
    }
  }
  
  paste(c(blocks, links), collapse = "\n")
}