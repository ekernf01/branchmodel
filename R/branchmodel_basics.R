## ------------------------------------------------------------------------
setMethod( "nrow", signature(x = "branchmodel"), function(x) {return(nrow(x@raw_data))} )
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

#' Plot the data and the branchmodel object on a scatterplot (returned ggplot2 plot).
#'
#' Currently just plots the first two variables. 
#' Eventually might switch to plotting a projection onto a cleverly chosen plane.
#' @export
plot_branchmodel = function( branchmodel, main = "" ){
  
  # Plot the points
  par = as.data.frame( rbind( branchmodel@center, branchmodel@tips) )
  embedding = branchmodel@raw_data[, 1:2]
  names( par ) = names(embedding) = colnames(branchmodel@raw_data)
  p = ggplot2::ggplot( ) + ggplot2::ggtitle( main ) +
    ggplot2::geom_point(data = cbind( embedding, 
                                      branch = branchmodel@assignments ),
                        aes_string( x = colnames(branchmodel@raw_data)[1], 
                                    y = colnames(branchmodel@raw_data)[2], 
                                    colour = "factor( branch )" ) ) +
    ggplot2::geom_point( aes_string( x = colnames(branchmodel@raw_data)[1], 
                                     y = colnames(branchmodel@raw_data)[2]),
                         colour = "black", data = par) 
  p = p + scale_color_manual( values = c("grey", scales::hue_pal()(3)))
  
  # Early in the iteration, this function might get called on a branchmodel with the @models slot not filled yet.
  if(length(branchmodel@models) != 3){
    return(p)
  }
  
  # Add the branch models as lines.
  pc_to_plot = c()
  for( i in 1:3){
    pc = branchmodel@models[[i]]
    data_i = data.frame( pc$s[pc$tag, 1:2] )
    data_i = cbind(data_i, branch = i)
    pc_to_plot = rbind( pc_to_plot, data_i)
  }
  p = p + ggplot2::geom_line ( aes_string( x = colnames(branchmodel@raw_data)[1], 
                                           y = colnames(branchmodel@raw_data)[2],
                                           colour = "factor( branch )",
                                           group  = "factor( branch )" ), 
                               data = pc_to_plot )
  
  return(p)
}

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

