#' A QQ plot
#'
#'
#' @param data The dataset only containing the data coloumns correlations between.
#' @param type
#' @param df
#' @param ylabel
#' @param xlabel
#' @keywords
#' @examples
#' @import tidyverse
#' @export
qqplot <- function(data = NULL,
                        type = "normal",
                        df = NULL,
                        ylabel = "Observed Values",
                        xlabel = "Theoretical Normality Quantiles") {

  if ((TRUE %in% (
    class(data) == "tbl" |
    class(data) == "data.frame" |
    class(data) == "tbl_df")))
    data = unlist(data, use.names = FALSE)
  if (!(TRUE %in% (
    class(data) == "numeric"|
    class(data) == "double"
  )))
    return("Must enter a numeric data vector.")


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
  return(list(dataset = DataNorm, graph = qqplot))
}

