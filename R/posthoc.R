#' Bonferonni-Holm Correction
#'
#' This function computes the bonferroni-holm correction and determines if the p-values
#' entered are significant or not.
#'
#' @param p_vals Enter a vector of p-values to compute the bonferroni-holm correction on.
#' @param alpha The alpha level of you significance test.
#' @return
#' \item{nSig}{The number of significant results.}
#' \item{p_val}{The significant p-values.}
#' \item{n}{The amount of p-values corrected. The same as the length of your input vector.}
#' \item{data}{A dataset containing the p-values, their respective adjusted alpha level,
#' aswell as a boolean indicating if the p-value is significant or not.}
#' @export
bonholm_cor <- function(p_vals, alpha = .05) {
  if (!is.numeric(p_vals)) {
    stop("You must enter a numeric vector.")
  }
  sorted_pvals <- sort(p_vals)
  sorted_pvals
  n <- length(sorted_pvals)
  adjusted_alpha <- alpha / (n + 1 - seq_len(n))
  non_significant <- sorted_pvals > adjusted_alpha
  non_significant
  first_accept <- which(non_significant == TRUE)[1]
  significant <- sorted_pvals[seq_len(first_accept - 1)]
  return(list(nSig = length(significant),
              p_val = significant,
              n = n,
              data = tibble(p_vals = sorted_pvals,
                            alpha = adjusted_alpha,
                            signi = !non_significant)))
}
