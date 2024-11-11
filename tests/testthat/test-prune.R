# library(testthat)
# library(dendextend)

context("prune.R")


test_that("prune_leaf works", {

   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   # if duplicate label
   duplicated_dend <- dend
   labels(duplicated_dend)[1] <- labels(duplicated_dend)[2]
   expect_warning(
      result <- prune_leaf(duplicated_dend, "Florida")   
   )
   expect_identical(
      result,
      duplicated_dend
   )
   
})
