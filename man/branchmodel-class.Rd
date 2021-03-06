% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/branchmodel.R
\docType{class}
\name{branchmodel-class}
\alias{branchmodel-class}
\alias{new_branchmodel_helper}
\title{Return an empty branchmodel object.}
\description{
Return an empty branchmodel object.
}
\section{Slots}{

\describe{
\item{\code{raw_data:}}{One row is one datum. Stores coords in some space.}

\item{\code{dist_df:}}{One row is one datum. Stores distances from  each branch of the latent 'Y' shape (three columns).}

\item{\code{assignments:}}{length is number of data points. Entries give rows of `tips` corresponding to the closest branch.}

\item{\code{tips:}}{Matrix with one row per branch (three total). One column per variable (same dimension as `raw_data`).}

\item{\code{models:}}{List with one element per branch (three total). Currently supports only `principal.curve` objects from the `princurve` package.}

\item{\code{center:}}{atomic of the same dimension as `raw_data[1, ]`.}
}}

