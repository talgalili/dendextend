# library(testthat)
# library(dendextend)


context("find_dend")


test_that("find_dend works", {
  
   x <- datasets::mtcars
   dist_x <- dist(x)
   
   best_dend <- find_dend(x, dist_methods = c("euclidean"))
   best_dist_dend <- find_dend(dist_x)
   
   # should be the same, as dist() resorts to euclidean by default
   expect_equal(
      best_dend,
      best_dist_dend
   )
   
})
