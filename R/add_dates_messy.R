#' Generate "dates from hell" (mixed formats)
#'
#' Create a character vector of dates/times in a variety of inconsistent,
#' hard-to-parse formats (useful for teaching data cleaning).
#'
#' @param n Integer, number of dates to generate.
#' @param start,end Character or Date; inclusive range to sample from
#'   (defaults: "2018-01-01" .. "2025-12-31").
#' @param p_na Proportion of empty strings to inject (default 0.02).
#' @param seed Optional integer seed for reproducibility.
#' @param probs Optional named probabilities for formats. Names must be from:
#'   \itemize{
#'     \item \code{iso}           — YYYY-MM-DD
#'     \item \code{ymd_slash}     — YYYY/MM/DD
#'     \item \code{dmy_slash}     — DD/MM/YYYY
#'     \item \code{mdy_slash}     — MM/DD/YYYY
#'     \item \code{dmy_dot}       — DD.MM.YYYY
#'     \item \code{mon_d_yyyy}    — Mon DD, YYYY  (e.g., "Aug 17, 2025")
#'     \item \code{d_mon_yyyy}    — DD Mon YYYY   (e.g., "17 Aug 2025")
#'     \item \code{rfc2822}       — RFC-2822 string with time/zone
#'     \item \code{iso_datetime}  — ISO 8601 with time and zone
#'     \item \code{excel_serial}  — Excel serial (Windows origin 1899-12-30)
#'     \item \code{epoch_secs}    — Unix epoch seconds (UTC midnight-ish)
#'   }
#'   Unspecified formats share remaining mass equally.
#'
#' @return Character vector of length \code{n} with mixed-format date strings.
#' @export
add_dates_messy <- function(
    n,
    start = "2022-01-01",
    end   = "2025-12-31",
    p_na  = 0.02,
    seed  = NULL,
    probs = NULL
) {
  stopifnot(n > 0)
  if (!is.null(seed)) set.seed(seed)
  
  # base date range
  start <- as.Date(start)
  end   <- as.Date(end)
  if (end < start) stop("`end` must be >= `start`.")
  base_dates <- start + sample.int(as.integer(end - start + 1L), n, replace = TRUE) - 1L
  
  # default probabilities (roughly realistic mix)
  fmts <- c("iso","ymd_slash","dmy_slash","mdy_slash","dmy_dot",
            "mon_d_yyyy","d_mon_yyyy","iso_datetime")
  p <- rep(1/length(fmts), length(fmts)); names(p) <- fmts
  if (!is.null(probs)) {
    # override provided, renormalize remainder evenly
    if (!all(names(probs) %in% fmts)) {
      stop("Unknown format names in `probs`. Allowed: ", paste(fmts, collapse = ", "))
    }
    p[names(probs)] <- probs
    if (any(p < 0)) stop("`probs` cannot be negative.")
    # renormalize to sum 1
    p <- p / sum(p)
  }
  
  # choose a format per row
  fmt_pick <- sample(fmts, n, replace = TRUE, prob = p)
  
  # helper: random time and zone
  rand_time <- function(n) {
    hh <- sprintf("%02d", sample(0:23, n, TRUE))
    mm <- sprintf("%02d", sample(0:59, n, TRUE))
    ss <- sprintf("%02d", sample(0:59, n, TRUE))
    paste0(hh, ":", mm, ":", ss)
  }
  tz_pool <- c("UTC","GMT","CET","CEST","EST","EDT","PST","PDT","+0100","+0200","Z")
  
  out <- character(n)
  # Excel origin (Windows): 1899-12-30
  excel_origin <- as.Date("1899-12-30")
  
  for (i in seq_len(n)) {
    d <- base_dates[i]
    f <- fmt_pick[i]
    out[i] <- switch(f,
                     iso           = format(d, "%Y-%m-%d"),
                     ymd_slash     = format(d, "%Y/%m/%d"),
                     dmy_slash     = format(d, "%d/%m/%Y"),
                     mdy_slash     = format(d, "%m/%d/%Y"),
                     dmy_dot       = format(d, "%d.%m.%Y"),
                     mon_d_yyyy    = format(d, "%b %e, %Y"),             # "Aug 17, 2025" (note space-padded day)
                     d_mon_yyyy    = format(d, "%d %b %Y"),              # "17 Aug 2025"
                     iso_datetime  = paste0(format(d, "%Y-%m-%d"), "T", rand_time(1), sample(c("Z","+01:00","+02:00","-05:00"), 1))
    )
  }
  
  # inject a few empties (true from-hell)
  if (p_na > 0) {
    k <- max(0, round(p_na * n))
    if (k > 0) {
      idx <- sample.int(n, k)
      out[idx] <- ""
    }
  }
  
  return(out)
}
