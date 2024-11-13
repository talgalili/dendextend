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


test_that("prune works for non-dendrogram objects", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   ph <- ape::as.phylo(hc)
   
   # should achieve the same result regardless of object type
   result <- prune.hclust(hc, "Florida")
   expect_equal(
      labels(as.dendrogram(result)),
      labels(prune_leaf(dend, "Florida"))
   )
   result <- prune.phylo(ph, "Florida")
   expect_equal(
      labels(as.dendrogram(result)),
      labels(prune_leaf(dend, "Florida"))
   )
   # rpart is not a dependency for dendextend. so long as that is true, this will create an error in tests
   # however, for local machine testing, rpart is likely installed. so, to force an error we use a non-tree object
   expect_error(
      prune.rpart(1:4, -1000)
   )
   
   # no default for prune
   expect_error(
      prune.default(dend)
   )
})


test_that("prune_common_subtrees.dendlist works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend1 <- as.dendrogram(hc)
   dend2 <- shuffle(dend1)
   dend12 <- dendlist(dend1, dend2)
   
   # dend1 and dend2 have the same subtrees, so resulting dend1 should be unchanged
   result <- prune_common_subtrees.dendlist(dend12)
   expect_identical(
      result[[1]],
      dend1      
   )
   
   # if wrong objects passed in
   expect_error(
      prune_common_subtrees.dendlist(dend12[1])
   )
   expect_error(
      prune_common_subtrees.dendlist(list(dend1, dend2))
   )
   
})


test_that("intersect_trees works", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend1 <- as.dendrogram(hc)
   hc <- USArrests[2:11, ] %>%
      dist() %>%
      hclust() 
   dend2 <- as.dendrogram(hc)   
   
   # if there is an issue with label overlap
   expect_warning(
      intersect_trees(dend1, dend2, warn = T)   
   )
   hc <- USArrests[11:20, ] %>%
      dist() %>%
      hclust() 
   dend2 <- as.dendrogram(hc)   
   expect_warning(
      intersect_trees(dend1, dend2, warn = T)   
   )
   
})