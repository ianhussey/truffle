#' Inject Missingness (Clean MCAR or Messy Encodings)
#'
#' Introduce missingness into columns (default: those whose names begin with `"X"`).
#' Two modes are supported:
#' \itemize{
#'   \item \strong{Clean MCAR} (\code{dirtier = FALSE}): uses
#'     \code{missMethods::delete_MCAR()} to replace a proportion of cells with
#'     genuine \code{NA} values (no type changes).
#'   \item \strong{Messy encodings} (\code{dirtier = TRUE}): replaces a proportion
#'     of cells with a mixture of encodings (e.g., \code{NA}, \code{"NA"}, \code{-99},
#'     \code{-999}, empty strings, \code{"missing"}). Targeted columns are coerced
#'     to \emph{character} to accommodate mixed types—useful for teaching data
#'     cleaning.
#' }
#'
#' @param .data A data frame.
#' @param prop Numeric in \[0,1\]. Proportion of cells to replace
#'   in each targeted column.
#' @param dirtier Logical. If \code{FALSE}, use clean MCAR via
#'   \code{missMethods::delete_MCAR()}. If \code{TRUE}, inject mixed encodings
#'   (columns become character). Default \code{TRUE}.
#' @param pool Vector of replacement tokens used when \code{dirtier = TRUE}.
#'   Defaults to \code{c(NA, "NA", -99, -999, "", "missing")}.
#' @param probs Optional numeric vector of the same length as \code{pool}
#'   giving replacement probabilities (will be renormalized). If \code{NULL},
#'   uses equal probabilities.
#' @param col_pattern Regular expression for selecting columns to modify
#'   (default \code{"^X"} = columns whose names start with \code{"X"}).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A modified data frame:
#' \itemize{
#'   \item If \code{dirtier = FALSE}: same column types as input; MCAR \code{NA}s inserted.
#'   \item If \code{dirtier = TRUE}: targeted columns are coerced to \emph{character} and
#'         contain a mix of encodings (e.g., \code{NA}, \code{"NA"}, \code{-99}, etc.).
#' }
#'
#' @details
#' When \code{dirtier = TRUE}, approximately \code{round(prop * n)}
#' cells per targeted column are replaced (where \code{n} is that column's length).
#' The selection is independent across columns. Coercion to character preserves
#' \code{NA} as \code{NA_character_} while other tokens are inserted as strings.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' df <- data.frame(id = 1:6,
#'                  X1 = sample(1:5, 6, TRUE),
#'                  X2 = sample(1:5, 6, TRUE),
#'                  Y  = rnorm(6))
#'
#' # Clean MCAR: only genuine NA, numeric types preserved
#' clean <- dirt_missingness(df, prop = 0.2, dirtier = FALSE)
#'
#' # Messy encodings: columns X* become character with mixed "missing" tokens
#' messy <- dirt_missingness(df, prop = 0.2, dirtier = TRUE,
#'                           pool = c(NA, "NA", -99, ""),  # customize as needed
#'                           seed = 42)
#' }
#'
#' @seealso \code{\link[missMethods]{delete_MCAR}}, \code{\link[tidyselect]{starts_with}}
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect starts_with vars_select
#' @importFrom missMethods delete_MCAR
#' @importFrom stats runif
dirt_missingness <- function(.data,
                             prop = 0.05,
                             dirtier = TRUE,
                             pool = c(NA, "NA", -99, -999, "", "missing"),
                             probs = NULL,
                             col_pattern = "^X",
                             seed = NULL) {
  stopifnot(is.data.frame(.data))
  if (!is.null(seed)) set.seed(seed)
  
  if(!dirtier){
    
    output <- delete_MCAR(.data,
                          p = prop,
                          cols_mis = vars_select(colnames(.data), starts_with("X")))
    return(output)
    
  } else if (dirtier) {
    
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
    output <- dplyr::mutate(
      .data,
      dplyr::across(
        .cols = tidyselect::starts_with(sub("^\\^", "", col_pattern)),  # allow "^X" or "X"
        .fns = ~ {
          n <- length(.x)
          k <- max(0L, round(prop * n))
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
    return(output)
  }
}
