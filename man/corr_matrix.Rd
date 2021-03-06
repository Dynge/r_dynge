% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr_matrix.R
\name{corr_matrix}
\alias{corr_matrix}
\title{A Correlation Matrix function}
\usage{
corr_matrix(data = NULL, type = "pearson", colors = c("steelblue2",
  "firebrick"))
}
\arguments{
\item{data}{The dataset only containing the coloumns you want correlations between.}

\item{type}{The correlation type, use either "pearson" or "spearman".}

\item{colors}{A vector containing the colors (either names or #Codes) of the colors you want to use.
The first index is equal a negative correlation, second index equals positive correlation.}
}
\value{
\item{data}{Calculated correlations aswell as p-values.}
\item{plot}{The ggplot object.}
}
\description{
This function allows you to create a correlation plot aswell as get correlational data.
}
\examples{
rpdp_corr_matrix(mtcars, "pearson")
}
\keyword{correlation}
