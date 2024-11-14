# library(testthat)
# library(dendextend)


context("all.equal")


test_that("has_nodePar works", {
   hc <- hclust(dist(1:5))
   
   # if non-dendrogram object passed in
   expect_true(
      !has_nodePar(hc)   
   )
   
})