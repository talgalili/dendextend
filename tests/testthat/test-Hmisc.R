# library(testthat)
# library(dendextend)


context("as.dendrogram.varclus")


test_that("as.dendrogram.varclus works", {
  
   object <- list()
   hc <- hclust(dist(mtcars))
   object$hclust <- hc
   
   expect_identical(
      as.dendrogram.varclus(object),
      as.dendrogram(hc)
   )
       
})
