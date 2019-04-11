#' Mode function
#'
#' This function allows you to calculate the mode of a vector.
#' The mode is the most often value in a vector.
#' @param vector Vector of the data you want to calculate the mode of.
#'
#' @return
#' \item{mode} The value most often found in the vector.
#'
#' @example
#' Mode(mtcars$cyl)
#'
#' @export
Mode <-
  function(vector) {
    uniq_vals <- unique(vector)
    counts <- sapply(uniq_vals, function(uniq_val) sum(vector == uniq_val))
    mode_val <- uniq_vals[which.max(counts)]
    return(mode_val)
  }
