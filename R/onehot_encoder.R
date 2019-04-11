#' A One-Hot Encoder
#'
#' This functions allows you to transform a vector into a One-Hot Encoded matrix.
#' Useful for nominal input data and tensorflow neural network outputs.
#'
#' @param factor_vector Vector containing the factors you want to one-hot encode.
#' @param name Name of the coloumns in the output matrix.
#'
#' @return
#' \item{one-hot_matrix} Encoded matrix for the factors.
#'
#' @keywords onehot-encoding
#' @example
#' onehot_encode(mtcars$cyl, "Cylinders")
#' @export
onehot_encode <- function(factor_vector, name) {
  classes <- unique(factor_vector)
  matrix_cols <- length(classes)
  matrix_rows <- length(factor_vector)
  onehot_matrix <- matrix(0, ncol = matrix_cols, nrow = matrix_rows)
  class_idx <- lapply(classes, function(class) which(factor_vector == class))
  for (i in seq_along(class_idx)) {
    onehot_matrix[class_idx[[i]], i] = 1
  }
  colnames(onehot_matrix) <- sapply(seq_len(matrix_cols),
                                    function(x) paste(name, classes[x], sep = "."))
  return(onehot_matrix)
}
