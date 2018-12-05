#' Cross-Validation Splitting
#'
#' This function shuffles the observations of a dataset, splits it into k-folds and produces a
#'  list of the training sets and a list of the validation sets. Furthermore
#' @param train The dataset you wish to split.
#' @param folds The amount of folds you wish to produce. Note the amount of observations divided by folds must equal a whole number.
#' @return
#' \item{trainlist}{A list of the training sets}
#' \item{vallist}{A list of the validation sets}
#' @examples
#' data(iris)
#' cv_lists <- cvsplit(iris)
#' @export
cvsplit <- function(train = NULL,
               folds = 10) {
  if (!(TRUE %in% (
    class(train) == "tbl" |
    class(train) == "data.frame" |
    class(train) == "tbl_df"
  ))) {
    stop("It would seem I have not received a dataset, but something else...")
  }
  if (nrow(train) / folds != as.integer(nrow(train) / folds)) {
    stop("The training set and amount of folds does not match.
 The training set divided by the folds must equal a whole number."
    )
  }
  if (nrow(train) / folds < 10) {
    stop("Your training set is too small for this about of folds.
 I do not allow for validation sets smaller than 10 in size.
 Try lowering the amount of folds or achieve a bigger training set."
    )
  }
  validation_size <- nrow(train) / folds
  split_seq <- seq(1, nrow(train) + 1, validation_size)
  shuffle <- sample(nrow(train))
  train <- train[shuffle, ]
  trainlist <- lapply(seq_len(folds),
                      function(x) train[-c(split_seq[x]:(split_seq[x + 1] - 1)), ])
  vallist <- lapply(seq_len(folds),
                      function(x) train[c(split_seq[x]:(split_seq[x + 1] - 1)), ])
  return(list(trainlist = trainlist, vallist = vallist))

  }
