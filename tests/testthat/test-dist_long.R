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


test_that("labels<-.dist works", {
   
   sample_matrix <- matrix(1:9, nrow = 3)
   dist_object <- dist(sample_matrix)
   label_vec <- c("A", "B", "C")
   labels(dist_object) <- label_vec
   
   expect_identical(
      attr(dist_object, "Labels"), 
      label_vec
   )

})
