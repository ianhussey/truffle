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