# library(testthat)
# library(dendextend)


context("rainbow_fun")


test_that("rainbow_fun works", {
   
   dend <- as.dendrogram(hclust(dist(mtcars)))

   # if colorspace not installed
   expect_no_error(with_mocked_bindings(
      rainbow_fun(1),
      requireNamespace = function(...) FALSE
   ))
   
})