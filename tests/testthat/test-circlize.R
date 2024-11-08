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
   mock_requireNamespace <- function(package, ...) {
      if (package == "circlize") return(FALSE)
      base::requireNamespace(package, ...)
   }
   mockery::stub(circlize_dendrogram, "requireNamespace", mock_requireNamespace)
   expect_error(
      circlize_dendrogram(dend)
   )
   # undo changes that were needed to mock circlize not being installed
   undo_mock_requireNamespace <- function(package, ...) {
      base::requireNamespace(package, ...)
   }
   mockery::stub(circlize_dendrogram, "requireNamespace", undo_mock_requireNamespace)

})


