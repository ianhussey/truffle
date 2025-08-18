#' Add a messy date column (pipe-friendly)
#'
#' Appends a column of mixed-format dates/times to a data frame, suitable for
#' teaching data cleaning. Designed to be used inside tidy pipelines.
#'
#' @param .data A data frame.
#' @param col Name of the column to create (string or bare name). Default: "date".
#' @param start,end Character or Date; inclusive date range to sample from
#'   (defaults: "2022-01-01" .. "2025-12-31").
#' @param p_na Proportion of empty strings to inject (default 0.02).
#' @param seed Optional integer seed for reproducibility.
#' @param probs Optional named probabilities for formats. Names must be from:
#'   \code{c("iso","ymd_slash","dmy_slash","mdy_slash","dmy_dot","mon_d_yyyy","d_mon_yyyy","iso_datetime")}.
#'
#' @return A data frame equal to \code{dat} with the new messy date column appended.
#' @examples
#' \dontrun{
#' dat <- generate_data_likert_two_conditions(... )
#' dat <- dat |>
#'   dirt_dates(col = "date")     # or dirt_dates(date)
#' }
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang ensym :=
dirt_dates <- function(
    .data,
    col   = "date",
    start = "2022-01-01",
    end   = "2025-12-31",
    p_na  = 0.02,
    seed  = NULL,
    probs = NULL
) {
  stopifnot(is.data.frame(.data))
  n <- nrow(.data)
  nm <- rlang::ensym(col)
  
  if (!is.null(seed)) set.seed(seed)
  
  # base date range
  start <- as.Date(start)
  end   <- as.Date(end)
  if (end < start) stop("`end` must be >= `start`.")
  base_dates <- start + sample.int(as.integer(end - start + 1L), n, replace = TRUE) - 1L
  
  # formats + probs
  fmts <- c("iso","ymd_slash","dmy_slash","mdy_slash","dmy_dot",
            "mon_d_yyyy","d_mon_yyyy","iso_datetime")
  p <- rep(1/length(fmts), length(fmts)); names(p) <- fmts
  if (!is.null(probs)) {
    if (!all(names(probs) %in% fmts)) {
      stop("Unknown names in `probs`. Allowed: ", paste(fmts, collapse = ", "))
    }
    p[names(probs)] <- probs
    if (any(p < 0)) stop("`probs` cannot be negative.")
    p <- p / sum(p)
  }
  
  # helper: random time
  rand_time <- function(m) {
    hh <- sprintf("%02d", sample(0:23, m, TRUE))
    mm <- sprintf("%02d", sample(0:59, m, TRUE))
    ss <- sprintf("%02d", sample(0:59, m, TRUE))
    paste0(hh, ":", mm, ":", ss)
  }
  
  fmt_pick <- sample(fmts, n, replace = TRUE, prob = p)
  out <- character(n)
  
  for (i in seq_len(n)) {
    d <- base_dates[i]
    f <- fmt_pick[i]
    out[i] <- switch(
      f,
      iso           = format(d, "%Y-%m-%d"),
      ymd_slash     = format(d, "%Y/%m/%d"),
      dmy_slash     = format(d, "%d/%m/%Y"),
      mdy_slash     = format(d, "%m/%d/%Y"),
      dmy_dot       = format(d, "%d.%m.%Y"),
      mon_d_yyyy    = format(d, "%b %e, %Y"),
      d_mon_yyyy    = format(d, "%d %b %Y"),
      iso_datetime  = paste0(format(d, "%Y-%m-%d"), "T",
                             rand_time(1),
                             sample(c("Z","+01:00","+02:00","-05:00"), 1))
    )
  }
  
  # inject empties
  if (p_na > 0) {
    k <- max(0, round(p_na * n))
    if (k > 0) out[sample.int(n, k)] <- ""
  }
  
  dplyr::mutate(.data, !!nm := out)
}
