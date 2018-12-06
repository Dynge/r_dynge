#' A Correlation Matrix function
#'
#' This function allows you to create a correlation plot aswell as get correlational data.
#' @param data The dataset only containing the coloumns you want correlations between.
#' @param type The correlation type, use either "pearson" or "spearman".
#' @param colors A vector containing the colors (either names or #Codes) of the colors you want to use.
#' The first index is equal a negative correlation, second index equals positive correlation.
#' @return
#' \item{data}{Calculated correlations aswell as p-values.}
#' \item{plot}{The ggplot object.}
#'
#' @keywords correlation
#' @import tidyverse Hmisc reshape2
#' @examples
#' rpdp_corr_matrix(mtcars, "pearson")
#' @export
corr_matrix <-
  function(data = NULL,
           type = "pearson",
           colors = c("steelblue2", "firebrick")) {
    if (!(TRUE %in% (
      class(data) == "tbl" |
      class(data) == "data.frame" |
      class(data) == "tbl_df"
    )))
      stop("Must enter a dataset")

    CorrData <- round(cor(data, method = type), 2)

    get_upper_tri <- function(CorrData) {
      CorrData[lower.tri(CorrData, diag = TRUE)] <- NA
      return(CorrData)
    }
    upper_tri <- get_upper_tri(CorrData)

    PValCorr <- Hmisc::rcorr(as.matrix(data), type = type)
    PValCorr <- PValCorr$P
    PVal.upper_tri <- get_upper_tri(PValCorr)

    melted_PValCorr <- reshape2::melt(PVal.upper_tri, na.rm = TRUE)
    CorrData <- reshape2::melt(upper_tri, na.rm = TRUE)
    CorrData <- rename(CorrData, Corr = value)
    CorrData$p <- melted_PValCorr$value

    CorrPlot <- ggplot(CorrData, aes(Var2, Var1, fill = Corr)) +
      geom_tile(color = back.col) +
      theme_dynge() +
      scale_fill_gradient2(
        low = colors[1],
        high = colors[2],
        mid = back.col,
        midpoint = 0,
        limit = c(-1, 1),
        space = "Lab",
        name = "Pearson\nCorrelation  "
      ) +
      xlab("") +
      ylab("") +
      theme(panel.grid.major.y = element_line(colour = "grey90"))
    return(list(data = CorrData, plot = CorrPlot))
  }


