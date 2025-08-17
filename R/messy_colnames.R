#' Subtly Ruin Column Names
#'
#' Intentionally degrade a proportion of column names with small corruptions
#' while keeping them mostly recognizable (so they can be repaired in exercises).
#' Corruptions include partial case changes, inserted spaces/dots, added or
#' dropped characters, prefixes/suffixes, occasional duplicates or reserved names.
#'
#' @param data A data.frame or tibble.
#' @param prop Proportion of column names to corrupt (0–1). Default 0.5.
#' @param seed Optional integer seed for reproducibility.
#' @param allow_duplicates If TRUE, may introduce duplicate names (default TRUE).
#' @param reserved_prob Small probability to replace a ruined name with a
#'   reserved/awkward name like "TRUE" or "0 invalid". Default 0.03.
#'
#' @return The same data with corrupted column names. Adds attribute
#'   \code{"name_map"} containing a data.frame with \code{old} and \code{new}.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' dat <- data.frame(
#'   id = 1:3,
#'   extroversion_sum_score = c(12, 15, 14),
#'   agreeableness_sum_score = c(10, 9, 13),
#'   reaction_time_ms = c(350, 420, 390)
#' )
#' bad <- messy_colnames(dat, prop = 0.6, seed = 123)
#' attr(bad, "name_map")
#' }
messy_colnames <- function(
    data,
    prop = 0.25,
    seed = NULL,
    allow_duplicates = TRUE,
    reserved_prob = 0.03
) {
  stopifnot(is.data.frame(data))
  if (!is.null(seed)) set.seed(seed)
  
  nms <- names(data)
  n <- length(nms)
  k <- max(0, min(n, round(prop * n)))
  if (k == 0) return(data)
  
  pick <- sample(seq_len(n), k, replace = FALSE)
  
  # helpers (base R; small, partial corruptions)
  cap1 <- function(x) paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))
  to_camel_partial <- function(s) {
    parts <- unlist(strsplit(s, "[^A-Za-z0-9]+"))
    if (length(parts) <= 1) return(cap1(s))
    paste0(tolower(parts[1]), paste0(cap1(parts[-1]), collapse = ""))
  }
  swap_sep <- function(s) {
    # replace some underscores with spaces or dots (not all)
    s <- sub("_", " ", s)        # first occurrence to space
    s <- sub("_", ".", s)        # next occurrence to dot
    s
  }
  insert_space <- function(s) {
    if (nchar(s) < 3) return(paste0(s, " "))
    pos <- sample(2:(nchar(s)-1), 1)
    paste0(substr(s,1,pos), " ", substr(s,pos+1,nchar(s)))
  }
  drop_chars <- function(s) {
    L <- nchar(s); if (L <= 4) return(s)
    m <- sample(1:2, 1)
    idx <- sort(sample(2:(L-1), m))  # avoid first char to keep recognizability
    paste0(strsplit(s, "")[[1]][-idx], collapse = "")
  }
  add_noise <- function(s) {
    pool_pre <- c("", "", "", " ", "0 ", "~", "#")
    pool_suf <- c("", "", "", " *", " (ms)", " v2", "_old")
    paste0(sample(pool_pre, 1), s, sample(pool_suf, 1))
  }
  tweak_case <- function(s) {
    # apply a case style but not fully: maybe title first token only, or camelize base
    if (grepl("_", s)) return(to_camel_partial(s))
    # otherwise randomly title-case a prefix
    parts <- strsplit(s, " ")[[1]]
    if (length(parts) > 1) {
      parts[1] <- cap1(tolower(parts[1]))
      return(paste(parts, collapse = " "))
    }
    cap1(tolower(s))
  }
  make_reserved <- function() sample(c("TRUE", "FALSE", "NULL", "NA", "Inf", "0 invalid"), 1)
  
  new <- nms
  for (i in pick) {
    s <- nms[i]
    # apply 2–3 random subtle operations
    ops <- sample(c("sep", "space", "drop", "noise", "case"), sample(2:3,1))
    for (op in ops) {
      s <- switch(op,
                  sep   = swap_sep(s),
                  space = insert_space(s),
                  drop  = drop_chars(s),
                  noise = add_noise(s),
                  case  = tweak_case(s),
                  s
      )
    }
    # occasionally force a reserved/bad name
    if (runif(1) < reserved_prob) s <- make_reserved()
    
    new[i] <- s
  }
  
  # optionally create duplicates by copying some new names
  if (allow_duplicates && n >= 2 && runif(1) < 0.25) {
    m <- max(1, round(0.1 * n))
    dup_from <- sample(seq_len(n), m, replace = TRUE)
    dup_to   <- sample(seq_len(n), m, replace = FALSE)
    new[dup_to] <- new[dup_from]
  }
  
  # assign and attach map
  names(data) <- new
  attr(data, "name_map") <- data.frame(old = nms, new = new, stringsAsFactors = FALSE)
  return(data)
}
