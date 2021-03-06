% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/branchmodel.R
\name{initialize_tips}
\alias{initialize_tips}
\title{Set up the initial guess for the branchmodel. Inspired by kmeans++.}
\usage{
initialize_tips(branchmodel)
}
\description{
Set up the initial guess for the branchmodel. Inspired by kmeans++.
}
\details{
Fix a tip randomly. Take the farthest point from that, and the farthest point from 
those two, and then reset the random one to the farthest from the other two.
Distance to the pair is the minimum over the individual distances.
}
