# library(testthat)
# library(dendextend)


context("circlize")


test_that("circlize_dendrogram works", {
   
   dend <- as.dendrogram(hclust(dist(mtcars)))
   
   # normal case
   expect_no_error(
      circlize_dendrogram(dend, labels_track_height = NA, labels = F)   
   )
   
   # if duplicate labels
   labels(dend)[2] <- labels(dend)[1]
   expect_warning(
      circlize_dendrogram(dend)
   )
   # if circlize not installed
   expect_error(with_mock(
      requireNamespace = function(...) FALSE,
      circlize_dendrogram(dend)
   ))
   
})


