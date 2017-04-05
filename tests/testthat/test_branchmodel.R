
# # Unit tests for distance_to_line_segment
testthat::test_that("distance_to_line_segment is correct", {

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
})

testthat::test_that("get_connected_component is correct", {

  expect_equal( get_connected_component( matrix(c(1:5, 5:1), ncol = 2), 5), c(5, 1) )
  three_plus_three = matrix(c(1, 2, 3,
                              2, 3, 1,
                              3, 2, 1,
                              4, 5, 6,
                              6, 5, 4,
                              5, 4, 6), ncol = 3, byrow = T)
  expect_equal( get_connected_component( three_plus_three, 1), 1:3 )
  expect_equal( get_connected_component( three_plus_three, 4), 4:6 )
})

# Set up some test data
set.seed(which(LETTERS=="Y"))
test_Y = data.frame( x1 = c( 1:100, 51:100 ) + 3*rnorm(150),
                     y1 = c( 1:100, 50:1  ) + 3*rnorm(150) )
c1 = data.frame( x = rnorm(50) + 01,  y = rnorm(50) + 02 )
c2 = data.frame( x = rnorm(50) + 05,  y = rnorm(50) + 05 )
c3 = data.frame( x = rnorm(50) + 05,  y = rnorm(50) + 10 )
c4 = data.frame( x = rnorm(50) + 10,  y = rnorm(50) + 05 )

testthat::test_that("find_contiguous_region is reasonable", {
  idx = find_contiguous_region(  all_points = rbind( c1, c2, c3, c4 ),
                                 good_idx = c(1:50, 101:150), root_idx = 1)
  expect_equal(sort(idx), 1:50)
} )


testthat::test_that("Empty branches are successfully reinitialized",
                    {
                      orig_assign = branchmodel1@assignments
                      branchmodel1@assignments[branchmodel1@assignments==1] = as.integer(0)
                      branchmodel1 = reassign_points(branchmodel1)
                      assertthat::are_equal(orig_assign, branchmodel1@assignments)
                    })

branchmodel1 =  fit_branchmodel( test_Y )
show( branchmodel1 )
plot_branchmodel( branchmodel1 )

branchmodel2 =  fit_branchmodel( raw_data = rbind( c1, c2, c3, c4 ) )
show( branchmodel2 )
plot_branchmodel( branchmodel2 )

data("messy_demo")
branchmodel3 =  fit_branchmodel( raw_data = messy_demo )
plot_branchmodel( branchmodel3 )

