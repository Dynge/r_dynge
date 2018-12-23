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
#' @examples
#' p_vals <- lapply(seq_len(ncol(mtcars)), function(x) shapiro.test(mtcars[[x]]))
#' adjust_p <- bonholm(p_vals)
#' adjust_p
#' @export
bonholm <- function(p_vals, alpha = .05) {
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
  first_accept <- if(is.na(first_accept)) {
    length(sorted_pvals) + 1
  } else {
    first_accept
  }
  significant <- sorted_pvals[seq_len(first_accept - 1)]
  return(list(
    nSig = length(significant),
    p_val = significant,
    n = n,
    data = tibble(
      p_vals = sorted_pvals,
      alpha = adjusted_alpha,
      signi = !non_significant
    )
  ))
}


#' Pariwise T-Tests
#' @export
posthoc_ttest <- function(long = NULL,
                          grouping = NULL,
                          wide = NULL,
                          paired = FALSE) {
  if (is.null(long) & is.null(grouping)) {
    if (!is.data.frame(wide)) {
      stop("You must enter either a dataframe in wide format, or a vector of the scores aswell as a vector of the groupings.")
    } else {
      method = "wide"
    }
  } else {
    if (!is.numeric(long)) {
      stop("The input vector must be numeric in long format.")
    } else {
      method = "long"
    }
  }
  if (method == "wide") {
    groups <- lapply(seq_len(ncol(wide)), function(x)
      wide[[x]])
    tests <-
      mapply(function(x, y)
        t.test(groups[[x]], groups[[y]])$p.value,
        seq_len(ncol(wide)),
        sort(rep(seq_len(ncol(wide)), ncol(wide))))
    tests <- matrix(tests, ncol = ncol(wide))
    tests[!lower.tri(tests)] <- NA
    bh_correction <- bonholm(tests)
    output <- list(p.val_matrix = tests,
                   BH = bh_correction)
  } else {
    groups <- tapply(long, grouping, function(x) x)
    tests <-
      mapply(function(x, y)
        t.test(groups[[x]], groups[[y]])$p.value,
        seq_along(groups),
        sort(rep(seq_along(groups), length(groups))))
    tests <- matrix(tests, ncol = length(groups))
    tests[!lower.tri(tests)] <- NA
    bh_correction <- bonholm(tests)
    output <- list(p.val_matrix = tests,
                   BH = bh_correction)
  }


#SOMETHING IS VERY WRONG IN THIS ONE
  return(output)
}
