###Fit a 'Y' shape to your data

This R package uses an alternating approach to fit branching models to data. 
	
#####Install

Unfortunately, I am a newbie with S4 classes and R packages, so `branchmodel` won't work unless you load in some dependencies. 

	devtools::install_github("ekernf01/branchmodel")
	library( ggplot2 )
	library( assertthat )
	library( princurve )
	library( FNN )
	library( kknn )
	library( kernlab )

#####Usage 

Feed it a dataframe with numeric columns.

	data(demo_branch)
	head(demo_branch)
	demo_mod = fit_branchmodel( demo_branch )
	plot_branchmodel( demo_mod )