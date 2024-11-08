# library(testthat)
# library(dendextend)


context("cor.dendlist")


test_that("cor.dendlist works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   dend1 <- as.dendrogram(hclust(dist(mtcars[1:5, ]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(mtcars[1:5, ]), method = "ward.D2"))
   dend12 <- dendlist(dend1, dend2)
   
   expect_identical(
      round(cor.dendlist(dend12, "cophenetic"), 3),
      matrix(c(1.000, 0.711, 0.711, 1.000), nrow = 2)   
   )
   expect_identical(
      round(cor.dendlist(dend12, "baker"), 3),
      matrix(c(1.000, 0.701, 0.701, 1.000), nrow = 2)   
   )
   expect_identical(
      round(cor.dendlist(dend12, "common_nodes"), 3),
      matrix(c(1.000, 0.889, 0.889, 1.000), nrow = 2)   
   )
   expect_identical(
      round(cor.dendlist(dend12, "FM_index", k = 2), 3),
      matrix(c(1.000, 0.408, 0.408, 1.000), nrow = 2)   
   )
   
   # if non-dendrogram object is passed in
   x <- dist(mtcars)
   expect_error(
      cor.dendlist(x)   
   )
   
})


test_that("cor_FM_index works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   dend1 <- as.dendrogram(hclust(dist(mtcars[1:5, ]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(mtcars[1:5, ]), method = "ward.D2"))
   
   
   # if k isn't specified
   expect_error(
      cor_FM_index(dend1, dend2)   
   )
   # if all leaves part of the same cluster
   expect_warning(with_mock(
      cutree = function(dend, k, ...) rep(0, length(labels(dend))),
      cor_FM_index(dend1, dend2, k = 2)
   ))
   
})
