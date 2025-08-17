#' Discretize Continuous Data into Likert Scales by Condition
#'
#' This function converts continuous variables into Likert-style ordinal data 
#' while preserving mean differences between experimental conditions. 
#' Shared cutpoints are applied across groups to ensure comparability.
#'
#' @param data A data frame or matrix of continuous variables (e.g., from 
#'   \code{lavaan::simulateData}).
#' @param n_levels Integer or vector specifying the number of Likert levels 
#'   per column (default = 7).
#' @param labels Either \code{NULL}, a character vector of category labels of 
#'   length equal to \code{n_levels}, or a list of such vectors (one per column).
#' @param ordered Logical; whether the output should be returned as ordered 
#'   factors (default = TRUE).
#' @param method Character; either \code{"per_column"} (z-standardize each 
#'   column separately) or \code{"fixed"} (apply fixed reference 
#'   \code{mu_ref}, \code{sd_ref}). Default is \code{"per_column"}.
#' @param mu_ref Numeric; the mean used for fixed standardization (default = 0).
#' @param sd_ref Numeric; the standard deviation used for fixed standardization 
#'   (default = 1).
#'
#' @details
#' When \code{method = "per_column"}, each variable is standardized within 
#' columns before discretization. This removes mean differences across groups.  
#' When \code{method = "fixed"}, a shared reference mean and standard deviation 
#' are used, preserving mean shifts across groups.
#'
#' @return A data frame with discretized Likert-style variables.
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' mod <- "F1 =~ x1 + x2 + x3"
#' dat <- simulateData(mod, sample.nobs = 100)
#' likert_dat <- continuous_to_likert_by_condition(
#'   dat,
#'   n_levels = 5,
#'   ordered = TRUE,
#'   method = "fixed",
#'   mu_ref = 0,
#'   sd_ref = 1
#' )
#' head(likert_dat)
#' }
#'
#' @export
#' @importFrom stats sd dnorm
#' @importFrom latent2likert discretize_density
continuous_to_likert_by_condition <- function(data,
                                              n_levels = 7,
                                              labels   = NULL,
                                              ordered  = TRUE,
                                              method   = c("per_column", "fixed"),
                                              mu_ref   = 0,
                                              sd_ref   = 1) {
  method <- match.arg(method)
  stopifnot(is.matrix(data) || is.data.frame(data))
  X <- as.data.frame(data)
  p <- ncol(X)
  
  # recycle n_levels if scalar
  if (length(n_levels) == 1) n_levels <- rep(n_levels, p)
  stopifnot(length(n_levels) == p)
  
  # precompute endpoints per unique K
  Ks <- unique(n_levels)
  endp_list <- lapply(Ks, function(K) {
    latent2likert::discretize_density(density_fn = stats::dnorm, n_levels = K)$endp
  })
  names(endp_list) <- as.character(Ks)
  
  # normalise labels input
  lab_list <- vector("list", p)
  if (!is.null(labels)) {
    if (is.list(labels)) {
      stopifnot(length(labels) == p)
      lab_list <- labels
    } else if (is.character(labels)) {
      for (j in seq_len(p)) {
        stopifnot(length(labels) == n_levels[j])
        lab_list[[j]] <- labels
      }
    } else {
      stop("labels must be NULL, a list (length = ncol(data)), or a character vector of length n_levels.")
    }
  }
  
  out <- X
  for (j in seq_len(p)) {
    xj <- X[[j]]
    
    if (method == "per_column") {
      mu  <- mean(xj, na.rm = TRUE)
      sdj <- stats::sd(xj, na.rm = TRUE)
      if (!is.finite(sdj) || sdj <= 0) stop("Column ", j, " has non-positive SD.")
      z <- (xj - mu) / sdj
    } else { # fixed reference
      if (!is.finite(sd_ref) || sd_ref <= 0) stop("sd_ref must be > 0.")
      z <- (xj - mu_ref) / sd_ref
    }
    
    endp   <- endp_list[[as.character(n_levels[j])]]
    labs_j <- if (length(lab_list[[j]]) == 0) as.character(seq_len(n_levels[j])) else lab_list[[j]]
    
    yj <- cut(z, breaks = endp, labels = labs_j,
              include.lowest = TRUE, right = TRUE, ordered_result = ordered)
    if (!ordered) yj <- as.integer(yj)
    
    out[[j]] <- yj
  }
  out
}