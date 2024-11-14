# library(testthat)
context("Baker's gamma correlation between two trees")


test_that("lowest_common_branch works", {
  item1 <- structure(c(1L, 1L, 1L, 1L), .Names = c("1", "2", "3", "4"))
  item2 <- structure(c(1L, 1L, 2L, 2L), .Names = c("1", "2", "3", "4"))
  expect_identical(lowest_common_branch(item1, item2), 2)
  # if no shared items
  item1 <- structure(c(1L, 1L, 1L, 1L), .Names = c("1", "2", "3", "4"))
  item2 <- structure(c(2L, 2L, 2L, 2L), .Names = c("1", "2", "3", "4"))
  expect_identical(lowest_common_branch(item1, item2), 0)
})


test_that("cor_bakers_gamma works", {
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(23235)
  ss <- sample(1:150, 10) # we want to compare small trees
  hc1 <- hclust(dist(datasets::iris[ss, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[ss, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  #    cutree(dend1)

  # hclust and dendrogram works:
  expect_identical(cor_bakers_gamma(hc1, hc2), cor_bakers_gamma(dend1, dend2))
  expect_identical(round(cor_bakers_gamma(dend1, dend2), 2), 0.57)

  # use_labels_not_values works:
  expect_identical(
    cor_bakers_gamma(dend1, dend2, use_labels_not_values = TRUE),
    cor_bakers_gamma(dend1, dend2, use_labels_not_values = FALSE)
  )

  # Cor value of a tree with itself is 1:
  expect_identical(round(cor_bakers_gamma(dend1, dend1), 2), 1)

  # tree order has no effect on the correlation:
  rev_dend1 <- rev(dend1)
  expect_identical(round(cor_bakers_gamma(dend1, rev_dend1), 2), 1)

  # But labels order does matter!!
  dend1_mixed <- dend1
  labels(dend1_mixed) <- rev(labels(dend1_mixed))
  # does NOT mean having the worst cor!
  expect_false(identical(cor_bakers_gamma(dend1, dend1_mixed), 1))
  
  expect_identical(round(cor_bakers_gamma.default(dend1, dend2), 2), 0.57)
  
  dend12 <- dendlist(dend1, dend2)
  expect_identical(round(cor_bakers_gamma.dendlist(dend12), 2), 0.57)
})


test_that("bakers_gamma_for_2_k_matrix works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(23235)
   ss <- sample(1:150, 10) # we want to compare small trees
   hc1 <- hclust(dist(datasets::iris[ss, -5]), "com")
   hc2 <- hclust(dist(datasets::iris[ss, -5]), "single")
   dend1 <- as.dendrogram(hc1)
   dend2 <- as.dendrogram(hc2)
   
   k_matrix_dend1 <- cutree(dend1, k = 1:nleaves(dend1))
   k_matrix_dend2 <- cutree(dend2, k = 1:nleaves(dend2))
   
   expect_equal(
      bakers_gamma_for_2_k_matrix(k_matrix_dend1, k_matrix_dend1, to_plot = T),
      1
   )
   
   # if COR_object is NA
    trace("bakers_gamma_for_2_k_matrix", quote(COR_object <- NA), at = 9, print = FALSE)
   expect_no_error(
      bakers_gamma_for_2_k_matrix(k_matrix_dend1, k_matrix_dend1)
   )
   untrace("bakers_gamma_for_2_k_matrix")
   
   # if dimension misalignment
   expect_error(
      bakers_gamma_for_2_k_matrix(k_matrix_dend1, k_matrix_dend2[-1,])   
   )
   # if all labels aren't shared
   capture.output(expect_error(
      bakers_gamma_for_2_k_matrix(k_matrix_dend1[-2,], k_matrix_dend2[-1,])
   ))
   
})
