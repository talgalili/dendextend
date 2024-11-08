# library(testthat)
# library(dendextend)


context("seriate_dendrogram")


test_that("seriate_dendrogram works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   x <- dist(mtcars)
   dend <- as.dendrogram(hclust(x))
   
   seriated_dend <- seriate_dendrogram(dend, x)
   expect_identical(
      labels(seriated_dend)[1:3],
      c("Honda Civic", "Toyota Corolla", "Fiat 128")   
   )
   
   # if labels in x not identical to labels in dend
   labels(x)[1] = labels(x)[2]
   expect_error(
      seriate_dendrogram(dend, x)
   )
   # if x is not a distance object
   fake_x <- 1:4
   expect_error(
      seriate_dendrogram(dend, fake_x)
   )
   # if non-dendrogram or non-hclust object passed in
   fake_dend <- 1:4
   expect_error(
      seriate_dendrogram(fake_dend)
   )
   # if seriation not installed
   expect_error(with_mock(
      requireNamespace = function(...) FALSE,
      seriate_dendrogram(dend)
   ))
   
})
