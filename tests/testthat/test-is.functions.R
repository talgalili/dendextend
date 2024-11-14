# library(testthat)
# library(dendextend)


context("is.functions")


test_that("is.phylo works", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   phy <- ape::as.phylo(dend)
   
   expect_true(
      is.phylo(phy)
   )
})


test_that("fac2num works", {
   # if non-factor object passed in
   expect_error(
      fac2num(1:4)
   )
})