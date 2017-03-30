# All the real code is in R markdown files.

library( ggplot2 )
library( assertthat )
library( matrixStats )
library( princurve )

knitr::purl("R/branchmodel.Rmd", output = "R/branchmodel.R")
source("R/branchmodel.R")

knitr::purl("R/branchmodel_basics.Rmd", output = "R/branchmodel_basics.R")
source("R/branchmodel_basics.R")

knitr::purl("R/branchmodel_numerical.Rmd", output = "R/branchmodel_numerical.R")
source("R/branchmodel_numerical.R")
