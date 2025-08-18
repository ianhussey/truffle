#' Check Generated Data (flexible number of scales)
#'
#' Checks a generated dataset by computing (i) Cohen's \emph{d} per scale
#' between two groups in \code{condition}, (ii) the correlation matrix among
#' scale sum scores, and (iii) item-level histograms by condition.
#' The number of scales is discovered from column names (no need to be fixed at 3).
#'
#' @param .data A data frame containing item-level Likert data and a
#'   \code{condition} column with exactly two groups. Optional columns
#'   \code{id}, \code{age}, \code{gender} are ignored in score creation.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{cohens_d}}{Named numeric vector: Cohen's \emph{d} per scale sum score.}
#'   \item{\code{r}}{Correlation matrix among scale sum scores.}
#'   \item{\code{item_histograms}}{A ggplot object with histograms faceted by item and condition.}
#' }
#'
#' @examples
#' \dontrun{
#' out <- truffle_check(dat)
#' out$cohens_d
#' out$r
#' print(out$item_histograms)
#' }
#'
#' @export
#' @importFrom dplyr select any_of bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap
#' @importFrom stats na.omit cor
truffle_check <- function(.data) {
  stopifnot(is.data.frame(.data))
  if (!("condition" %in% names(.data))) {
    stop("`.data` must contain a `condition` column with two groups.")
  }
  # Ensure two-group design
  if (length(unique(na.omit(.data$condition))) != 2L) {
    stop("`condition` must have exactly two non-missing levels.")
  }
  
  # 1) Build sum scores (discover scales automatically)
  item_block <- dplyr::select(.data, -dplyr::any_of(c("id","condition","gender","age")))
  sums_appended <- truffle_sum_scores_by_scale(item_block)
  sum_cols <- grep("_sum$", names(sums_appended), value = TRUE)
  if (length(sum_cols) == 0L) stop("No *_sum columns found after truffle_check().")
  
  # Keep only the sums; attach id and condition for analysis
  dat_sumcores <- dplyr::bind_cols(
    dplyr::select(.data, dplyr::any_of(c("id","condition"))),
    sums_appended[sum_cols]
  )
  
  # 2) Cohen's d per sum score
  cohens_d_function <- function(x, g) {
    # g expected to be 2 groups; handle NAs safely
    ok <- !(is.na(x) | is.na(g))
    x <- x[ok]; g <- droplevels(as.factor(g[ok]))
    if (nlevels(g) != 2L) return(NA_real_)
    m <- tapply(x, g, mean)
    s <- tapply(x, g, sd)
    n <- table(g)
    sp <- sqrt(((n[1]-1)*s[1]^2 + (n[2]-1)*s[2]^2) / (sum(n)-2))
    unname((m[2] - m[1]) / sp)  # group2 - group1
  }
  d_vec <- vapply(sum_cols, function(cc) {
    cohens_d_function(dat_sumcores[[cc]], dat_sumcores$condition)
  }, numeric(1))
  names(d_vec) <- sub("_sum$", "", sum_cols)
  d_vec <- round(d_vec, 2)
  
  # 3) Correlation matrix among sum scores
  r_mat <- cor(dat_sumcores[sum_cols], use = "pairwise.complete.obs")
  r_mat <- round(r_mat, 2)
  
  # 4) Item histograms by condition
  item_histograms <- .data |>
    tidyr::pivot_longer(
      cols = starts_with("X"),
      names_to = "item",
      values_to = "score"
    ) |>
    ggplot2::ggplot(ggplot2::aes(score)) +
    ggplot2::geom_histogram(binwidth = 1) +
    ggplot2::facet_wrap(item ~ condition)
  
  list(
    cohens_d = d_vec,
    r = r_mat,
    item_histograms = item_histograms
  )
}
