% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/branchmodel_numerical.R
\name{kknn_specClust_outlier_removal}
\alias{kknn_specClust_outlier_removal}
\title{Calls `kknn::specClust` with preprocessing to remove outliers and postprocessing to label them.}
\usage{
kknn_specClust_outlier_removal(data, centers, nn = 7, ...)
}
\value{
Atomic vector of length `nrow(data)` containing cluster labels.
}
\description{
Calls `kknn::specClust` with preprocessing to remove outliers and postprocessing to label them.
}
\details{
This is an attempt to circumvent this ARPACK failure issue: https://github.com/KlausVigo/kknn/issues/7
}
