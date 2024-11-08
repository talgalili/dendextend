# library(testthat)
# library(dendextend)


context("depth")


test_that("min_depth works", {
   
   dend <- as.dendrogram(hclust(dist(mtcars)))
   
   expect_identical(
      min_depth(dend),
      3
   )
   
   # if non-list object passed in
   x <- 1:4
   expect_identical(
      min_depth(x),
      1L
   )
   # if leaf is passed in as a list
   expect_error(
      min_depth(as.list(dend[[1]][[1]]))
   )
   
})


test_that("max_depth works", {
   
   dend <- as.dendrogram(hclust(dist(mtcars)))
   
   expect_identical(
      max_depth(dend),
      11
   )
   
   # if non-list object passed in
   x <- 1:4
   expect_identical(
      max_depth(x),
      1L
   )
   # if leaf is passed in as a list
   expect_error(
      max_depth(as.list(dend[[1]][[1]]))
   )
   
})
