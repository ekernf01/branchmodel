## ------------------------------------------------------------------------

# # Shitty little vector 2-norm shortcut
norm2 = function(x) { sqrt( sum( x*x ) ) }
hinge_loss = function( x ) ( x*(x>0) )
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

library( princurve )
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
