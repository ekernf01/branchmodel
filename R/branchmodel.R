## ------------------------------------------------------------------------
#' Return an empty branchmodel object.
#'
#' @slot raw_data: One row is one datum. Stores coords in some space.
#' @slot dist_df: One row is one datum. Stores distances from  each branch of the latent 'Y' shape (three columns).
#' @slot assignments: length is number of data points. Entries give rows of `tips` corresponding to the closest branch.
#' @slot tips: Matrix with one row per branch (three total). One column per variable (same dimension as `raw_data`).
#' @slot models: List with one element per branch (three total). Currently supports only `principal.curve` objects from the `princurve` package.
#' @slot center: atomic of the same dimension as `raw_data[1, ]`.
new_branchmodel_helper = setClass( "branchmodel",
                                   slots = c( raw_data      = "data.frame",
                                              dist_df       = "data.frame",
                                              assignments   = "integer",
                                              tips          = "matrix",
                                              tip_indices   = "integer",
                                              models        = "list",
                                              center        = "numeric" ) )


#' Check whether a branchmodel object is valid.
#'
#' @return Returns empty string if everything is fine. Otherwise, some other length-1 character.
setGeneric("get_issues", function( object, ... ) standardGeneric("get_issues"))
setMethod("get_issues", valueClass = "character", signature = signature(object = "branchmodel"), function(object) {
  issues = ""
  n_points  = nrow( object@raw_data )
  dimension = ncol( object@raw_data )
  if( length( object@center )      != dimension ){ issues = paste(issues, "dim_center" ) }
  if( ncol(   object@tips )        != dimension ){ issues = paste(issues, "dim_tips" ) }
  if( nrow(   object@dist_df )     != n_points  ){ issues = paste(issues, "nrow_dist_df" ) }
  if( length( object@assignments ) != n_points  ){ issues = paste(issues, "len_assignments" ) }
  if( nrow(   object@tips )        != 3         ){ issues = paste(issues, "num_tips" ) }
  if( length(   object@tip_indices ) != 3       ){ issues = paste(issues, "tip_indices" ) }
  if( length( object@models )      != 3         ){ issues = paste(issues, "num_models" ) }
  if( any( sapply( object@models, class ) != rep( "principal.curve", 3 ) ) ){ issues = paste(issues, "model_class" ) }
  if( ncol(   object@dist_df )     != 3         ){ issues = paste(issues, "dim_dist_df" ) }
  return( issues )
})

#' Set up the initial guess for the branchmodel. Inspired by kmeans++.
#'
#' @details Fix a tip randomly. Take the farthest point from that, and the farthest point from 
#' those two, and then reset the random one to the farthest from the other two.
#' Distance to the pair is the minimum over the individual distances.
initialize_tips = function( branchmodel ){
  
  min_dist_sq_y_z = function( x, y, z = NULL ) {
    d1 = distance_sq( x, y )
    if(is.null(z)){ return(d1) }
    d2 = distance_sq( x, z )
    return( min( d1, d2 ) )
  }
  tip3_idx = sample(size = 1, 1:nrow(branchmodel@raw_data))
  tip3_embedding = branchmodel@raw_data[tip3_idx, ]
  tip1_idx = which.max( apply( X = branchmodel@raw_data, MARGIN = 1, 
                               FUN = min_dist_sq_y_z, y = tip3_embedding ) )
  tip1_embedding = branchmodel@raw_data[tip1_idx, ]
  tip2_idx = which.max( apply( X = branchmodel@raw_data, MARGIN = 1, 
                               FUN = min_dist_sq_y_z, y = tip3_embedding, z = tip1_embedding ) )
  tip2_embedding = branchmodel@raw_data[tip2_idx, ]
  tip3_idx = which.max( apply( X = branchmodel@raw_data, MARGIN = 1, 
                               FUN = min_dist_sq_y_z, y = tip2_embedding, z = tip1_embedding ) )
  tip3_embedding = branchmodel@raw_data[tip3_idx, ]
  branchmodel@tips = as.matrix( Reduce( x = list( tip1_embedding, tip2_embedding, tip3_embedding ), f = rbind ) )
  branchmodel@tip_indices = c( tip1_idx, tip2_idx, tip3_idx)
  return( branchmodel )
}

#' For each point, fills in the distance to each line segment.
#' 
setGeneric( "get_simple_sq_distances", function( branchmodel, ... ) standardGeneric( "get_simple_sq_distances" ) )
setMethod("get_simple_sq_distances", valueClass = "branchmodel",
          signature = signature( branchmodel = "branchmodel" ),
          function             ( branchmodel ) {
  for(i in 1:nrow( branchmodel@tips ) ){
    dtip = function(x) distance_to_ray(tip1 = branchmodel@tips[i, ] , tip2 = branchmodel@center, point = x)
    branchmodel@dist_df[ , i ] = apply( X = branchmodel@raw_data[ , 1:2 ],
                                        FUN = dtip, MARGIN = 1 )
  }
  return( branchmodel )
})


## ------------------------------------------------------------------------
#' Fit a "Y" shape to data 
#'
#' @param raw_data Dataframe with numeric columns.
#' @param max_iter Default 20.
#' @param tol Stops when less than 100*tol percent of points are reclassified.
#' @return S4 object of class "branchmodel".
#' @details This function imposes a "Y" shape on data in a (preferably 2D) space.
#' It represents the shape as three principal curves (from `princurve`), which each point
#' hard-assigned to one curve. The internal methods iterate
#' between reassigning the data to the nearest branch and adjusting the branches,
#' with a heuristic to make the curves roughly meet in the center.
#' @export
fit_branchmodel = function( raw_data, max_iter = 100, tol = 0.01 ) {
  branchmodel = new_branchmodel_helper()
  branchmodel@raw_data = raw_data
  # # Initialize center to medioid and tips via kmeans++ ish
  branchmodel@center = unlist( apply( X = branchmodel@raw_data, FUN = median, MARGIN = 2 ) )
  branchmodel = initialize_tips( branchmodel )
  # # Distances to branches (using dist to rays from center through tips)
  branchmodel@dist_df = data.frame( matrix( NA, ncol = 3, nrow = nrow( branchmodel@raw_data ) ) )
  branchmodel = get_simple_sq_distances( branchmodel ) 
  # # Assign to nearest branch
  branchmodel = reassign_points( branchmodel ) 
  # # Iterate
  branchmodel = fit_branchmodel_internal( branchmodel, max_iter = max_iter, tol = tol )

  # # Check for issues and return
  issues = get_issues( branchmodel ) 
  assertthat::assert_that("" == issues )
  return( branchmodel )
}


## ------------------------------------------------------------------------
#' Internal branchmodel function that performs fitting.
#' 
setGeneric( "fit_branchmodel_internal", 
            function( branchmodel, max_iter, tol ) standardGeneric( "fit_branchmodel_internal" ) )
setMethod("fit_branchmodel_internal", valueClass = "branchmodel",
          signature = signature(branchmodel = "branchmodel", max_iter = "numeric", tol = "numeric"),
          function             (branchmodel, max_iter, tol ) {
  for( i in 1:max_iter ){
    old_assignments = branchmodel@assignments
    branchmodel = fit_branches( branchmodel )
    branchmodel = reassign_points( branchmodel )
    branchmodel = relocate_center( branchmodel, iter = i )
    # print( plot_branchmodel(branchmodel) )
    prop_just_reassigned = mean(old_assignments != branchmodel@assignments)
    if( prop_just_reassigned < tol ) { 
      print( paste0( "converged after ", i, " iterations" ) )
      break
    }
  }
  if( prop_just_reassigned >= tol ) { 
    warning( paste0( "Did not converge: prop_just_reassigned = ", prop_just_reassigned, ", max_iter = ", max_iter, "." ) ) 
  }
  return( branchmodel )
})

## ------------------------------------------------------------------------


#' Update the branch-point.
#'
#' @details This function updates `@center`. 
setGeneric( "relocate_center", function( branchmodel, iter, previous_max_ambiguity ) standardGeneric( "relocate_center" ) )
setMethod( "relocate_center", valueClass = "branchmodel",
           signature = signature( branchmodel = "branchmodel", iter = "numeric" ),
           function             ( branchmodel, iter ){
  if(any(branchmodel@assignments == 0)){
    branchmodel@center = colMeans( branchmodel@raw_data[branchmodel@assignments == 0, ])    
  } 
return( branchmodel )
})

#' Optimize individual branch models given branch assignments.
#' 
#' This function updates `@dist_df` and `@models`.
setGeneric( "fit_branches", function( branchmodel, ... ) standardGeneric( "fit_branches" ) )
setMethod( "fit_branches", valueClass = "branchmodel",
           signature = signature( branchmodel = "branchmodel" ),
           function             ( branchmodel ){
  # For each branch, fit a principal curve.
  # To make it touch the center, I augment the data with copies of the current center.
  # I remove them once the fitting has finished.
             
  for(i in 1:3){
    # augment with dummy data at center and fit to that plus unambiguous data
    this_cluster = which( branchmodel@assignments == i )
    n_aug = ceiling( 0.15*length( this_cluster ) ) 
    center_copies = t( matrix( branchmodel@center, ncol = n_aug, nrow = length( branchmodel@center ) ) )
    colnames( center_copies ) = colnames( branchmodel@raw_data )
    pc_input = rbind( branchmodel@raw_data[this_cluster, ], center_copies )
    branchmodel@models[[i]] = princurve::principal.curve( x = as.matrix(pc_input) )
    
    #remove dummy data
    branchmodel@models[[i]] = princurve_truncate( branchmodel@models[[i]], n_remove = n_aug )
    
    #get distances to curve
    projected_all = princurve::get.lam(x = as.matrix( branchmodel@raw_data ), 
                                       s = branchmodel@models[[i]]$s, 
                                       tag = branchmodel@models[[i]]$tag, 
                                       stretch = 2 )
    branchmodel@dist_df[, i] = apply( branchmodel@raw_data - projected_all$s, 1, norm2 )
  }

  return(branchmodel)
})

#' Reassign each point to the nearest branch. Assign ambiguous points the label 0.
#' 
#' This function updates `@assignments`.
setGeneric( "reassign_points", function( branchmodel, ... ) standardGeneric( "reassign_points" ) )
setMethod( "reassign_points", valueClass = "branchmodel",
           signature = signature( branchmodel = "branchmodel" ),
           function             ( branchmodel ){
             
  # # hard assignment
  branchmodel@assignments = apply( X = branchmodel@dist_df, MARGIN = 1, FUN = which.min )
  
  # # Find ambiguous points by comparing closest to second closest branch.
  # # 0 means completely ambiguous (closest two branches equal)
  # # -Inf means well determined: closest branch infinitely closer
  min2next = function(x) {
    x = sort(x)
    x[1] - x[2]
  }
  ambiguity = apply( X = branchmodel@dist_df, MARGIN = 1, FUN = min2next )
  sd = mean( apply( X = branchmodel@dist_df, MARGIN = 1, FUN = min ) )
  shared = ambiguity > -2*sd
  branchmodel@assignments[shared] = as.integer(0)

  # Don't ever change the assignment of the tips. Restart any empty branch at its tip.
  # If a cluster is empty, there must be at least two tips in the same 
  # cluster (by the pigeonhole principle), and one of these will be reassigned to the empty cluster.
  # Tip is accompanied by its 10 nearest neighbors, so that the princurve fit will have enough data.
  branchmodel@assignments[branchmodel@tip_indices[1:3]] = 1:3
  for( cluster in 1:3 ){
    if( sum(cluster == branchmodel@assignments) < 2 ){
      warning("Restarting empty branch! This is a bad sign for convergence. Check your results visually.\n")
      tip_assignments = branchmodel@assignments [ branchmodel@tip_indices ]
      cluster_hogging_tips = which.max( table( tip_assignments ) )
      neighbors = c( FNN::knnx.index( query = branchmodel@tips[ cluster ], 
                                      data = branchmodel@raw_data, k = 11 ) )
      branchmodel@assignments[neighbors] = cluster
    }
  }
  
  # Make sure assigned cells are in one contiguous block. Assign disconnected segments as ambiguous (0).
  for( cluster in 1:3 ){
    this_cluster_idx = which( branchmodel@assignments == cluster )
    conn_comp = find_contiguous_region( all_points = branchmodel@raw_data, 
                                        good_idx = this_cluster_idx, 
                                        root_idx = branchmodel@tip_indices[cluster] )
    indices_to_discard = setdiff( this_cluster_idx, conn_comp )
    branchmodel@assignments[ indices_to_discard ] = as.integer(0)
  }
 
  return( branchmodel )
})

