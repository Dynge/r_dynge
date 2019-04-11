#' A Root-Mean-Square Error function
#'
#' This function allows you to quickly calculate the RMSE of a error vector.
#' @param error Vector containing the difference between model predictions and true values.
#' @return
#' \item{rms}{Root Mean Square Error.}
#'
#' @keywords rmse
#' @examples
#' model <- lm(mpg~., data = mtcars)
#' predictions <- predict(model, mtcars)
#' error <- mtcars$mpg - predictions
#' rms <- rmse(error)
#' @export
rmse <-
  function(error){
    rms <- sqrt(mean(error^2))
    return(rms)
  }
