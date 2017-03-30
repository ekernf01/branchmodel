## ------------------------------------------------------------------------
setMethod( "nrow" , signature(x = "branchmodel"), function(x) {return(nrow(x@raw_data))} )
setMethod( "ncol", signature(x = "branchmodel"), function(x) {return(ncol(x@raw_data))} )

## ------------------------------------------------------------------------
# # Show only the dimensions of the data and the centers/tips
setMethod("show",
          signature = signature(object = "branchmodel"),
          definition = function( object ) {
            cat("An object of class ", class(object), "\n", sep = "")
            cat(" ", ncol(object), " variables and ",
                nrow(object), " samples.\n", sep = "")
            cat("Centered at: \n",  paste( object@center, collapse = ", "), "\n", sep = "")
            cat("Branch tips: \n"); print( object@tips )
            invisible(NULL)
          })

# # Plot the data and the branchmodel object on a scatterplot with figure title `main`.
# # Currently just plots the first two variables.
# # Eventually will switch to plotting a projection onto the plane containing the tips.
setGeneric( "plot_branchmodel", function( branchmodel, main ) standardGeneric( "plot_branchmodel" ) )
setMethod(  "plot_branchmodel", valueClass = "ggplot",
           signature = signature( branchmodel = "branchmodel", main = "character" ),
           function             ( branchmodel, main ){
  par = as.data.frame( rbind( branchmodel@center, branchmodel@tips) )
  embedding = branchmodel@raw_data[, 1:2]
  names( par ) = names(embedding) = c("X1", "X2")
  p = ggplot( ) + ggtitle( main ) +
    geom_point( aes( x = X1, y = X2, colour = factor( branch ) ),
                data = cbind( embedding,
                              branch = branchmodel@assignments ) )+
    geom_point( aes(x = X1, y = X2), colour = "black", data = par)

  pc_to_plot = c()

  # Return in case branch models have not been set up yet.
  if( length(branchmodel@models) < 3 ){
    return(p)
  }

  for( i in 1:3){
    pc = branchmodel@models[[i]]
    data_i = data.frame( pc$s[pc$tag, 1:2] )
    data_i = cbind(data_i, branch = i)
    pc_to_plot = rbind( pc_to_plot, data_i)
  }

  p = p + geom_line ( mapping = aes( colour = factor( branch ),
                                     group  = factor( branch ),
                                     x = x, y = y ),
                      data = pc_to_plot )

  return(p)
})

# # Subset the data.
setGeneric( "subset", function( branchmodel, index_keep ) standardGeneric( "subset" ) )
setMethod(  "subset", valueClass = "branchmodel",
           signature = signature( branchmodel = "branchmodel", index_keep = "integer"),
           function             ( branchmodel, index_keep ){
  branchmodel@raw_data    = branchmodel@raw_data   [ index_keep, ]
  branchmodel@dist_df     = branchmodel@dist_df    [ index_keep, ]
  branchmodel@assignments = branchmodel@assignments[ index_keep ]
  assertthat::assert_that( get_issues( branchmodel ) == "")
  return(branchmodel)
})

