## ------------------------------------------------------------------------

# # Shitty little vector 2-norm shortcut
norm2 = function(x) { sqrt( sum( x*x ) ) }
distance_sq =  function( x, y ) sum(( x - y )*( x - y ))
  
# # Calculates the distance between `point` and the closest convex combination
# # of `tip1` and `tip2`, i.e. the closest vector of the form
# # `p*tip1 + q*tip2` where 0 <= p, q <= 1; p + q = 1.
setGeneric( "distance_to_line_segment", function( tip1, tip2, point, ... ) standardGeneric( "distance_to_line_segment" ) )
setMethod( "distance_to_line_segment", valueClass = "numeric",
          signature = signature( tip1 = "numeric", tip2 = "numeric", point = "numeric" ),
          function             ( tip1, tip2, point ) {

  if( all( tip1 == point ) | all( tip2 == point ) ){ return(0) }
  point_centered = ( point - tip2 )
  tip1_centered  = ( tip1  - tip2 )
  point_centered_scaled  = point_centered / norm2 ( point_centered )
  tip1_centered_scaled   = tip1_centered  / norm2 ( tip1_centered  )
  c_cos_theta = sum( tip1_centered_scaled * point_centered_scaled ) * norm2 ( point_centered )
  comp_parallel =  c_cos_theta * tip1_centered_scaled

  if( 0 < c_cos_theta && c_cos_theta < norm2 ( tip1_centered  )  ){
    comp_perp = point_centered - comp_parallel
    return ( norm2( comp_perp ) )
  } else {
    endpoint_diffs = c( norm2 ( point - tip2 ), norm2 ( point - tip1 ) )
    return ( min ( endpoint_diffs ) )
  }
})


# # Calculates the distance between `point` and the closest vector of the form
# # `a( tip1 - tip2 ) + tip2` where a >= 0. 
setGeneric( "distance_to_ray", function( tip1, tip2, point, ... ) standardGeneric( "distance_to_ray" ) )
setMethod( "distance_to_ray", valueClass = "numeric",
          signature = signature( tip1 = "numeric", tip2 = "numeric", point = "numeric" ),
          function             ( tip1, tip2, point ) {

  if( all( tip1 == point ) | all( tip2 == point ) ){ return(0) }
  point_centered = ( point - tip2 )
  tip1_centered  = ( tip1  - tip2 )
  point_centered_scaled  = point_centered / norm2 ( point_centered )
  tip1_centered_scaled   = tip1_centered  / norm2 ( tip1_centered  )
  c_cos_theta = sum( tip1_centered_scaled * point_centered_scaled ) * norm2 ( point_centered )
  comp_parallel =  c_cos_theta * tip1_centered_scaled
  if( 0 < c_cos_theta ){
    comp_perp = point_centered - comp_parallel
    return ( norm2( comp_perp ) )
  } else {
    endpoint_diffs = c( norm2 ( point - tip2 ), norm2 ( point - tip1 ) )
    return ( min ( endpoint_diffs ) )
  }
})

#' Remove the last few points from a principal curve.
#' 
#' The tricky part is the tag, which is not ordered the same way as the others.
#' It's like the output of order(<positions along curve>) in that `pc$s[pc$tag, ]` is smooth.
#' Since it indexes `pc$s`, if I remove `pc$s[45,]`, need to make sure `!any(pc$tag==45)`.
setGeneric( "princurve_truncate", function( pc, n_remove, ... ) standardGeneric( "princurve_truncate" ) )
setMethod( "princurve_truncate", valueClass = "principal.curve",
          signature = signature( pc = "principal.curve", n_remove = "numeric" ),
          function             ( pc, n_remove ) {
  keep = nrow(pc$s) - n_remove ; keep = 1:keep
  
  pc$lambda  = pc$lambda[keep]
  pc$s       = pc$s     [keep, ]
  pc$tag     = intersect(pc$tag, keep)
  return( pc )
})

#' Find a connected component of a directed graph.
#'
#' @param neighbors neighbors directly reachable from each vertex, so there is an edge from `i` to `neighbors[i, j]`.
#' (This is built for knn graphs, hence the weird representation.)
#' @param start_idx Algorithm starts here and searches outward.
get_connected_component = function( neighbors, start_idx ){
  outside_edges = start_idx
  conn_comp_idx = start_idx
  for( i in 1:nrow(neighbors) ){
    conn_comp_idx = union( conn_comp_idx, outside_edges )
    outside_edges = c( neighbors[ outside_edges, ] ) #Move out a layer
    outside_edges = setdiff( outside_edges, conn_comp_idx ) #exclude things already searched
    if(length(outside_edges)==0){break}
  }
  conn_comp_idx = union( conn_comp_idx, outside_edges )
  return( conn_comp_idx )
}

#' Find the closest pair of points, one in one class and the other in another class.
#'
bipartite_closest_pair = function( coords_1, coords_2 ){
  knn_mod = FNN::get.knnx( query = coords_1, data = coords_2, k = 1 )
  nearest_ind_1 = which.min(knn_mod$nn.dist)
  nearest_ind_2 = knn_mod$nn.index[nearest_ind_1, 1]
  nearest_coords = rbind( coords_1[nearest_ind_1, ], coords_2[nearest_ind_2, ])
  return( list( inds = c(nearest_ind_1, nearest_ind_2), 
                coords = nearest_coords, 
                distance = knn_mod$nn.dist[nearest_ind_1, 1]) )
}

#' Calls `kknn::specClust` with preprocessing to remove outliers and postprocessing to label them.
#' 
#' @return Atomic vector of length `nrow(data)` containing cluster labels.
#' @details This is an attempt to circumvent this ARPACK failure issue: https://github.com/KlausVigo/kknn/issues/7
kknn_specClust_outlier_removal = function( data, centers, nn = 7, ...){
  # remove outliers
  dist_to_nn = rowSums( FNN::get.knn( data = data, k = nn, algorithm = c( "cover_tree" ) ) $nn.dist )
  cutoff = mean( dist_to_nn ) + 2*sd( dist_to_nn )
  is_outlier = dist_to_nn > cutoff
    
  # Cluster
  cluster_mod = kknn::specClust(data[!is_outlier, ], centers = centers, nn = nn,  ...)
  labels = rep(NA, nrow(data))
  labels[!is_outlier] = cluster_mod$cluster
  
  # label outliers
  labels[is_outlier] = FNN::knn( test  = data[ is_outlier, ], 
                                 train = data[!is_outlier, ], 
                                 cl = labels[!is_outlier], 
                                 k = nn, algorith = "cover_tree" )
  assertthat::assert_that(!any(is.na(labels)))
  return(labels)
}

#' Find a contiguous subset of points.
#' @param all_points Data used.
#' @param good_idx The "good" points are `all_points[good_idx, ]`.
#' @param root_idx The indices returned include this number.
#' @return An atomic vector of indexes such that `all_points[output, ]` is
#' contiguous and not interrupted by `all_points[-output, ]`.
#' @details Given a set of "good" points and a "root", find a 
#' group of good points that contains the root and doesn't span gaps
#' with nearby "bad" points.
find_contiguous_region = function( all_points, good_idx, root_idx ) {
  assertthat::assert_that( root_idx %in% good_idx )
  assertthat::assert_that( all( good_idx %in% 1:nrow( all_points ) ) )
  good_idx = unique( good_idx )
  
  # Notation convention:
  #
  # - variables ending in "_a" index `all_points`.
  # - variables ending in "_g" index `good_idx` or `good_points` or the cluster labels.
  # - variables ending in "_c" index `connected_set_g`.
  # - variables ending in "_s" index something else.
  
  root_idx_a = root_idx
  good_idx_a = good_idx
  is_good = rep("bad", nrow(all_points)); is_good[good_idx_a] = "good"; is_good = factor(is_good)
    
  # Subset good points and convert root index to access it
  good_points = all_points[good_idx_a, ]
  root_idx_g = which(root_idx==good_idx_a)
  assertthat::are_equal(good_points[root_idx_g, ], all_points[root_idx_a, ])
  
  # Set up many clusters and find the root cluster.
  num_clusters = ceiling(nrow( good_points ) / 30 )
  num_clusters = ifelse(num_clusters<2, 2, num_clusters)
  num_clusters = ifelse(num_clusters>30, 30, num_clusters)
  cluster_mod = kmeans( good_points, num_clusters )
  root_cluster_label = cluster_mod$cluster[root_idx_g]
  connected_set_g     = which(cluster_mod$cluster == root_cluster_label)
  not_connected_yet_g = which(cluster_mod$cluster != root_cluster_label)
  
  # Used during development
  #  plot_branchmodel(branchmodel)
  # qplot( x = good_points$branch_viz_1,
  #        y = good_points$branch_viz_2,
  #        colour = factor(cluster_mod$cluster) ) 

  # Iterate over the rest, incrementing the connected set.
  for( i in 1:( num_clusters - 1 ) ){
    # Update the root cluster and find the closest other cluster.
    nearest_points = FNN::get.knnx( query = good_points[ connected_set_g, ], 
                                    data  = good_points[ not_connected_yet_g, ], 
                                    k = 1 )
    best_query_c = which.min( nearest_points$nn.dist )
    best_datum_s = nearest_points$nn.index[best_query_c]
    closest_cluster_label  = cluster_mod$cluster[not_connected_yet_g[best_datum_s]]
    closest_cluster_idx_g    = which( closest_cluster_label == cluster_mod$cluster )

    # Does the gap between them have many bad points nearby?
    gap = bipartite_closest_pair( coords_1 = good_points[ connected_set_g, ], 
                                  coords_2 = good_points[ closest_cluster_idx_g, ] )
    assertthat::assert_that( gap$distance != 0 )
    gap = (gap$coords[1, , drop = F] + gap$coords[2, , drop = F]) / 2
    gap_nearby_points_a = c(FNN::get.knnx( query = gap, data = all_points, k = 20 )$nn.index)
    gap_nearby_assigments = table( is_good[gap_nearby_points_a] )
    gap_nearby_assigments = gap_nearby_assigments / sum(gap_nearby_assigments)
    
    # If yes, break and return current connected set. If no, increment connected set.
    if( gap_nearby_assigments[["bad"]] >= 0.25 ){
      break
    } else {
      connected_set_g     = union( connected_set_g, closest_cluster_idx_g )
      not_connected_yet_g = setdiff( 1:nrow( good_points ), connected_set_g )
      assertthat::assert_that(0==length(intersect(not_connected_yet_g, connected_set_g)))
    }
  }
  connected_set_a = good_idx[connected_set_g]
  assertthat::assert_that( is.atomic(  connected_set_a ) )
  assertthat::assert_that( is.numeric( connected_set_a ) )
  assertthat::assert_that( !any(is.na( connected_set_a ) ) )
  assertthat::assert_that( all( connected_set_a %in% 1:nrow( all_points ) ) )
  return( connected_set_a )
}

