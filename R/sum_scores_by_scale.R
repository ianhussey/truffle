sum_scores_by_scale <- function(
    data,
    id_regex = "^(X\\d+)",   # captures scale IDs like X1, X2, ...
    suffix   = "_sum",
    na_rm    = TRUE,         # ignore NAs when summing
    min_non_missing = 1,     # require this many non-missing items per scale
    prorate  = FALSE,        # prorate partial sums to full length
    add_counts = FALSE,      # also return counts of non-missing items
    count_suffix = "_n"
) {
  stopifnot(is.data.frame(data))
  nms <- colnames(data)
  
  # extract scale ID per column (e.g., "X1", "X2")
  id <- regmatches(nms, regexpr(id_regex, nms, perl = TRUE))
  if (anyNA(id) || any(id == "")) {
    stop("Some column names don't match id_regex = ", id_regex,
         ". Example names: ", paste(head(nms, 5), collapse = ", "))
  }
  
  # group column indices by scale ID
  idx <- split(seq_along(nms), id)
  
  # coerce Likert factors/ordered/characters to numeric before summing
  to_num <- function(x) {
    if (is.ordered(x) || is.factor(x)) return(as.integer(x))
    if (is.character(x)) return(suppressWarnings(as.numeric(x)))
    as.numeric(x)
  }
  
  sums_list <- list()
  counts_list <- list()
  
  for (nm in names(idx)) {
    cols <- idx[[nm]]
    X <- as.data.frame(lapply(data[ , cols, drop = FALSE], to_num))
    k <- ncol(X)
    
    # counts of non-missing per row
    n_nonmiss <- rowSums(!is.na(X))
    
    # raw sum (optionally ignoring NAs)
    s <- rowSums(X, na.rm = na_rm)
    
    # apply min_non_missing rule
    s[n_nonmiss < min_non_missing] <- NA_real_
    
    # prorate if requested: sum * (k / answered)
    if (prorate) {
      scale_factor <- ifelse(n_nonmiss > 0, k / n_nonmiss, NA_real_)
      s <- s * scale_factor
    }
    
    sums_list[[nm]] <- s
    counts_list[[nm]] <- n_nonmiss
  }
  
  # assemble data frame
  sums <- as.data.frame(sums_list)
  
  # order columns by numeric part of the ID (X1, X2, …)
  ord <- order(as.integer(sub("^X(\\d+)$", "\\1", names(sums))))
  sums <- sums[ord]
  names(sums) <- paste0(names(sums), suffix)
  
  if (add_counts) {
    counts <- as.data.frame(counts_list)[ord]
    names(counts) <- sub(paste0(suffix, "$"), "", names(sums))
    names(counts) <- paste0(names(counts), count_suffix)
    cbind(sums, counts, check.names = FALSE)
  } else {
    sums
  }
}