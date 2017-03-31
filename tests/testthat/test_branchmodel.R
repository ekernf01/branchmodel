
# # Unit tests for distance_to_line_segment
expect_equal( 1,       distance_to_line_segment( tip1 = c( 0,0 ),
                                                 tip2 = c( 0, 1 ),
                                                 point = c( 1, 1 ) ) )
expect_equal( sqrt(2), distance_to_line_segment( c( 0,0 ),
                                                 c( 0, 1 ),
                                                 c( 1, 2 ) ) )
expect_equal( sqrt(2), distance_to_line_segment( -c( 1, 1 ),
                                                 c( 2, 2 ),
                                                 c( 3, 3 ) ) )
expect_equal( sqrt(2*2.25), distance_to_line_segment( c( 1, 1 ),
                                                      c( 2, 2 ),
                                                      c( 0, 3 ) ) )


branchmodel1 =  fit_branchmodel( raw_data = data.frame( x1 = c( 1:50, 26:50 ) + 3*rnorm(75),
                                                        y1 = c( 1:50, 25:1  ) + 3*rnorm(75) ) )
show( branchmodel1 )
plot_branchmodel( branchmodel1 )

c1 = data.frame( x = rnorm(25) + 01,  y = rnorm(25) + 02 )
c2 = data.frame( x = rnorm(25) + 05,  y = rnorm(25) + 05 )
c3 = data.frame( x = rnorm(25) + 05,  y = rnorm(25) + 10 )
c4 = data.frame( x = rnorm(25) + 10,  y = rnorm(25) + 05 )

branchmodel2 =  fit_branchmodel( raw_data = rbind( c1, c2, c3, c4 ) )
show( branchmodel2 )
