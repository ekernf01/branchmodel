###Fit a 'Y' shape to your data

This R package uses an alternating approach to fit branching models to data. Feed it a dataframe with numeric columns.
	
	devtools::install_github("ekernf01/branchmodel")
	library(branchmodel)
	data(demo_branch)
	head(demo_branch)
	demo_mod = fit_branchmodel( demo_branch )
	plot_branchmodel( demo_mod )