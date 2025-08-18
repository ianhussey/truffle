#' Simulate reaction times (ms) via a shifted lognormal
#'
#' Generates plausible, right-skewed reaction times using a lognormal
#' distribution plus a constant nondecision-time shift.
#'
#' @param n Integer; number of RTs.
#' @param shift Nondecision-time shift in ms (default 200).
#' @param meanlog Mean of the lognormal on the log scale (default 5.5).
#' @param sdlog  SD of the lognormal on the log scale (default 0.35).
#' @param seed Optional integer for reproducibility.
#'
#' @return Numeric vector of reaction times (ms).
#' @export
#' @importFrom stats rlnorm
truffle_reaction_times <- function(n,
                                   shift = 200,
                                   meanlog = 7,
                                   sdlog  = 0.30,
                                   seed = NULL) {
  stopifnot(n > 0)
  if (!is.null(seed)) set.seed(seed)
  round(shift + rlnorm(n, meanlog = meanlog, sdlog = sdlog))
}
