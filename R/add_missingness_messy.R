#' Add Mixed "Messy" Missingness to X* Columns
#'
#' Randomly replaces a proportion of entries in columns whose names begin with
#' `"X"` using a mixture of true missing values and messy encodings:
#' `NA`, `"NA"`, sentinel codes like `-99`/`-999`, and `""` (empty string).
#' Replacements are done independently **per column**. Targeted columns are
#' coerced to character to accommodate mixed encodings.
#'
#' @param dat A data frame.
#' @param prop Proportion of cells to replace in each targeted column (0–1).
#'   Default `0.05`.
#' @param pool Vector of replacement tokens. Defaults to
#'   `c(NA, "NA", -99, -999, "")`.
#' @param probs Optional probabilities (same length as `pool`). If `NULL`,
#'   uses equal probabilities.
#' @param col_pattern Regex used to select columns (default: `"^X"`).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A modified data frame where matching columns are coerced to
#'   character and have approximately `prop` of their values replaced by
#'   mixed missingness encodings.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   id = 1:6,
#'   X1 = sample(1:5, 6, TRUE),
#'   X2 = sample(1:5, 6, TRUE),
#'   Y  = sample(1:5, 6, TRUE)
#' )
#' df_bad <- add_missingness_messy(df, prop = 0.15, seed = 42)
#' str(df_bad)   # note: X1/X2 are now character with mixed encodings
#' }
#'
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect starts_with
add_missingness_messy <- function(dat,
                                  proportion_missing = 0.05,
                                  pool = c(NA, "NA", -99, -999, "", "missing"),
                                  probs = NULL,
                                  col_pattern = "^X",
                                  seed = NULL) {
  stopifnot(is.data.frame(dat))
  if (!is.null(seed)) set.seed(seed)
  
  # normalize replacement probabilities
  if (is.null(probs)) {
    probs <- rep(1 / length(pool), length(pool))
  } else {
    stopifnot(length(probs) == length(pool))
    if (any(probs < 0)) stop("`probs` must be non-negative.")
    s <- sum(probs)
    if (s == 0) stop("`probs` must not sum to 0.")
    probs <- probs / s
  }
  
  # tidy verb: mutate across columns starting with pattern
  dplyr::mutate(
    dat,
    dplyr::across(
      .cols = tidyselect::starts_with(sub("^\\^", "", col_pattern)),  # allow "^X" or "X"
      .fns = ~ {
        n <- length(.x)
        k <- max(0L, round(proportion_missing * n))
        if (k == 0L) return(as.character(.x))
        
        idx <- sample.int(n, k, replace = FALSE)
        
        # draw replacements; coerce to character safely (preserve NA)
        repl <- sample(pool, k, replace = TRUE, prob = probs)
        repl_chr <- ifelse(is.na(repl), NA_character_, as.character(repl))
        
        y <- as.character(.x)
        y[idx] <- repl_chr
        y
      }
    )
  )
}