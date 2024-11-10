# library(testthat)
# library(dendextend)


context("stats_imports")


test_that("stats_.memberDend works", {
   x <- matrix(1:4)
   expect_identical(
      stats_.memberDend(x),
      1L
   )
})


test_that("stats_midcache.dendrogram works", {
   
   # create custom class to reach specific error
   dend <- as.dendrogram(hclust(dist(mtcars)))
   class(dend) <- c("custom_dend", class(dend))
   length.custom_dend <- function(x) {
      -1
   }
   
   expect_error(
      stats_midcache.dendrogram(dend)   
   )
   
})
