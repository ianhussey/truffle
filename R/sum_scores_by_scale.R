#' Compute Sum Scores by Scale
#'
#' This function computes participant-level sum scores for each scale 
#' based on item naming conventions (e.g., X1_item1, X1_item2, ...).
#' It can handle missingness and optional prorating.
#'
#' @param data A data.frame containing item-level data.
#' @param id_regex A regular expression with a capturing group for the scale ID
#'   (default: "^(X\\d+)").
#' @param suffix A suffix for the resulting sum-score column names (default: "_sum").
#' @param na_rm Logical; ignore NAs when summing (default TRUE).
#' @param min_non_missing Integer; minimum answered items required per scale (default 1).
#' @param prorate Logical; if TRUE, prorate sums by k/answered (default FALSE).
#' @param add_counts Logical; if TRUE, also return counts of non-missing items per scale.
#' @param count_suffix Suffix for the count columns (default "_n").
#'
#' @return A data.frame with one column per scale (and optionally counts).
#' @export
sum_scores_by_scale <- function(
    data,
    id_regex = "^(X\\d+)",   # captures scale IDs like X1, X2, ...
    suffix   = "_sum",
    na_rm    = TRUE,
    min_non_missing = 1,
    prorate  = FALSE,
    add_counts = FALSE,
    count_suffix = "_n"
) {
  stopifnot(is.data.frame(data))
  nms <- colnames(data)
  
  # 1) Keep only columns matching the regex and extract IDs (X1, X2, ...)
  sel <- grepl(id_regex, nms, perl = TRUE)
  if (!any(sel)) {
    stop("No columns match id_regex = ", id_regex,
         ". Example names: ", paste(head(nms, 5), collapse = ", "))
  }
  nms_sel <- nms[sel]
  ids     <- sub(paste0(id_regex, ".*"), "\\1", nms_sel, perl = TRUE)
  
  # 2) Group indices by extracted IDs (now many items map to one scale)
  idx <- split(which(sel), ids)
  
  # 3) Coercion helper
  to_num <- function(x) {
    if (is.ordered(x) || is.factor(x)) return(as.integer(x))
    if (is.character(x)) return(suppressWarnings(as.numeric(x)))
    as.numeric(x)
  }
  
  sums_list   <- vector("list", length(idx))
  counts_list <- vector("list", length(idx))
  names(sums_list) <- names(counts_list) <- names(idx)
  
  # 4) Compute row-wise sums (with optional missingness handling)
  for (nm in names(idx)) {
    cols <- idx[[nm]]
    X <- as.data.frame(lapply(data[, cols, drop = FALSE], to_num))
    k <- ncol(X)
    
    n_nonmiss <- rowSums(!is.na(X))
    s <- rowSums(X, na.rm = na_rm)
    
    s[n_nonmiss < min_non_missing] <- NA_real_
    
    if (prorate) {
      scale_factor <- ifelse(n_nonmiss > 0, k / n_nonmiss, NA_real_)
      s <- s * scale_factor
    }
    
    sums_list[[nm]]   <- s
    counts_list[[nm]] <- n_nonmiss
  }
  
  # 5) Assemble and order by numeric part of the ID
  sums <- as.data.frame(sums_list, check.names = FALSE)
  ord <- order(suppressWarnings(as.integer(sub("^X(\\d+)$", "\\1", names(sums)))))
  sums <- sums[ord]
  names(sums) <- paste0(names(sums), suffix)
  
  if (add_counts) {
    counts <- as.data.frame(counts_list, check.names = FALSE)[ord]
    names(counts) <- paste0(sub(paste0(suffix, "$"), "", names(sums)), count_suffix)
    cbind(sums, counts, check.names = FALSE)
  } else {
    sums
  }
}
