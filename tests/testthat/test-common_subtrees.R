# library(testthat)
suppressPackageStartupMessages(library(dendextend))

context("common subtrees")


test_that("rank_values_with_clusters works", {
  expect_equal(
    rank_values_with_clusters(c(1, 2, 3)),
    1:3
  )

  expect_equal(
    rank_values_with_clusters(c(1, 1, 3)),
    c(1L, 1L, 2L)
  )

  expect_equal(
    rank_values_with_clusters(c(0.1, 0.1, 3000)),
    c(1L, 1L, 2L)
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 2)),
    1:3
  )
  #
  expect_equal(
    rank_values_with_clusters(c(1, 3, 3, 3, 3, 3, 3, 4, 2, 2)),
    c(1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 4L, 4L)
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 2), ignore0 = TRUE),
    1:3
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 2), ignore0 = FALSE),
    1:3
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 0, 2), ignore0 = TRUE),
    c(1, 2, 0, 3)
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 0, 2), ignore0 = FALSE),
    1:4
  )
})


test_that("nodes_with_shared_labels works", {
   
   hc1 <- hclust(dist(mtcars[1:5, ]), method = "complete")
   dend1 <- as.dendrogram(hc1)
   hc2 <- hclust(dist(mtcars[1:5, ]), method = "ave")
   dend2 <- as.dendrogram(hc2)
   
   # shared nodes should match specified pattern
   expect_identical(
      nodes_with_shared_labels(dend1, dend2),
      c(F, T, T, T, T, T, F, T, T)
   )
   
})


test_that("replace_unique_items_with_0_and_rank works", {
   
   # replace '5' with 0 because it is unique, convert all other values to a rank
   x <- c(4,4,3,3,5)
   expect_identical(
      replace_unique_items_with_0_and_rank(x),
      c(1,1,2,2,0)
   )
})


test_that("common_subtrees_clusters works", {
   suppressWarnings(RNGversion("3.5.0"))
   
   hc1 <- hclust(dist(mtcars[1:5, ]), method = "complete")
   dend1 <- as.dendrogram(hc1)   
   hc2 <- hclust(dist(mtcars[1:5, ]), method = "ave")
   dend2 <- as.dendrogram(hc2)
   
   # three subtrees should be in common
   expect_identical(
      common_subtrees_clusters(dend1, dend2),
      c(1,1,1,0,0)
   )
   
   # temporarily redefine is.leaf to access and cover lines of code otherwise not possible
   set.seed(2)
   with_mock(
      is.leaf = function(x) sample(c(T,F), 1),
      common_subtrees_clusters(dend1, dend2)
   )
})
