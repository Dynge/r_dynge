#' A QQ plot
#'
#' Produces a QQ plot of your data with either normal or t-distributed quantiles.
#'
#' @param data The dataset only containing the data coloumns correlations between.
#' @param type The type of quantiles. Either "normal" or "t" distributed.
#' @param ylabel The label for the x axis.
#' @param xlabel The label for the y axis.
#' @return
#' \item{data}{A dataset containing the correlational values aswell as their respective p-values.}
#' \item{plot}{A ggplot object of the plot.}
#' @examples
#' data(mtcars)
#' qqplot(mtcars$mpg)
#' @import tidyverse
#' @export
qqplot <- function(data = NULL,
                        type = "normal",
                        ylabel = "Observed Values",
                        xlabel = "Theoretical Quantiles") {

  if ((TRUE %in% (
    class(data) == "tbl" |
    class(data) == "data.frame" |
    class(data) == "tbl_df")))
    data = unlist(data, use.names = FALSE)
  if (!(TRUE %in% (
    class(data) == "numeric"|
    class(data) == "double"
  )))
    stop("Must enter a numeric data vector.")


  n <- length(data)
  df <- n - 1
  sorted_data <- sort(data)
  DataNorm = tibble( x = sorted_data, zX = ( sorted_data - mean(sorted_data) ) / sd(sorted_data) )

  QTheory = seq(1 / (n * 2), 1 - 1 / (n * 2), 1 / n)
  if(type == "normal") q <- qnorm(QTheory, mean = 0, sd = 1)
  else if (type == "t") q <- qt(QTheory, df)
  DataNorm$q = q


  qqplot <- ggplot(DataNorm, aes(q, x)) +
    geom_point(alpha = .5, size = 3) +
    stat_function(
       fun = function(x)
         x * sd(data) + mean(data),
         col = "red",
         size = 1
      ) +
    theme_dynge() +
    xlab(xlabel) +
    ylab(ylabel)
  plot
  return(list(data = DataNorm, plot = qqplot))
}

