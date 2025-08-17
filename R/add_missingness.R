add_missingness <- function(dat, proportion_missing){ 
  delete_MCAR(
    dat,
    p = proportion_missing,
    cols_mis = vars_select(colnames(dat), starts_with("X"))
  )
}