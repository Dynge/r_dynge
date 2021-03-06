% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcafuncs.R
\name{pca_QoR}
\alias{pca_QoR}
\title{Quality of Representation}
\usage{
pca_QoR(model, labels = TRUE, dims = "default")
}
\arguments{
\item{model}{The PCA model from FactoMineR.}

\item{labels}{Boolean indicating if values should be plotted or not. Defaults to TRUE.}

\item{dims}{A vector of the dimensions you want to include e.g. c(1:5). Defaults to all dimensions in the PCA object.}
}
\value{
\item{data}{The data used to make the plot.}
\item{plot}{The ggplot object.}
}
\description{
This function plot the different attributes of your PCA model and
their respective quality of representation to the different principal components.
}
