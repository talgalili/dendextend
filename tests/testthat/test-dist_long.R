# library(testthat)
# library(dendextend)


context("dist_long")


test_that("dist_long works", {
   
   long_distance_matrix = 
   iris[2:6, -5] %>%
     dist() %>%
     dist_long()
   
   expect_identical(
      round(long_distance_matrix[1,3], 3),
      0.3
   )
   
})
