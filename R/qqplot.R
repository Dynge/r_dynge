#' A QQ plot
#'
#' Produces a QQ plot of your data with either normal or t-distributed quantiles.
#'
#' @param Y A data vector to plot on the Y axis.
#' @param X Additional data vector that will overwrite the normality quantiales that usually go on the X axis.
#' Useful to compare distributions of different vectors.
#' @param type The type of quantiles. Either "normal" or "t" distributed.
#' @param ylabel The label for the x axis.
#' @param xlabel The label for the y axis.
#' @return
#' \item{data}{A dataset containing the vectors used for the plot.}
#' \item{plot}{A ggplot object of the plot.}
#' @examples
#' data(mtcars)
#' qqplot(mtcars$mpg)
#'
#' data(mtcars)
#' qqplot(mtcars$mpg, mtcars$qsec)
#' @import tidyverse
#' @export
qqplot <- function(Y = NULL,
                   X = NULL,
                   type = "normal",
                   ylabel = "Observed Values",
                   xlabel = "Theoretical Quantiles") {
  if ((TRUE %in% (
    class(Y) == "tbl" |
    class(Y) == "data.frame" |
    class(Y) == "tbl_df"
  )))
    Y = unlist(Y, use.names = FALSE)
  if (!(TRUE %in% (class(Y) == "numeric" |
                   class(Y) == "double")))
    stop("Must enter a numeric data vector.")


  n <- length(Y)
  df <- n - 1
  sorted_Y <- sort(Y)

  if (is.null(X)) {
    QTheory = seq(1 / (n * 2), 1 - 1 / (n * 2), 1 / n)
    if (type == "normal") {
      X <- qnorm(QTheory, mean = 0, sd = 1)
    }
    else if (type == "t") {
      X <- qt(QTheory, df)
    }
    linefunc <- function() {
      stat_function(
        fun = function(x)
          x * sd(Y) + mean(Y),
        col = "red",
        size = 1
      )
    }
  } else {
    X = sort(X)
    linefunc <- function() {
      geom_smooth(method = "lm",
                  se = FALSE,
                  col = "red")

    }
  }

  plotdata = tibble(X = X, Y = sorted_Y)

  qqplot <- ggplot(plotdata, aes(X, Y)) +
    geom_point(pch = 1, size = 3) +
    linefunc() +
    theme_dynge() +
    xlab(xlabel) +
    ylab(ylabel)
  plot
  return(list(data = plotdata, plot = qqplot))
}
