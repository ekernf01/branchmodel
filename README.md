###Fit a 'Y' shape to your data

This R package was written in 2016 as a supplement to the popular single-cell RNA analysis tools Monocle and DPT. At that time, those tools did not return simple branch assignments for each cell: DPT did not assign branches at all and Monocle returned 15 to 20 branches, not 3, on the dataset we needed it for. 

This code uses an alternating approach to fit the simplest branching model to data, with each observation assigned to one segment of a "Y". 
	
#####Install

`branchmodel` requires some dependencies to be loaded manually with `library()`. 

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
