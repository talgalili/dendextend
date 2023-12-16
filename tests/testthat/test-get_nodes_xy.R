# library(testthat)
# library(dendextend)

context("Tests for get_nodes_xy function")
# Tests for get_nodes_xy function

test_that("get_nodes_xy returns correct dimensions", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   xy <- get_nodes_xy(dend)
   expect_equal(ncol(xy), 2)
   expect_equal(nrow(xy), nnodes(dend))
})


# TODO: it seems that the type argument doesn't make a difference.
#        it probably needs to be removed.
test_that("get_nodes_xy handles different types correctly", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   xy_rectangle <- get_nodes_xy(dend, type = "rectangle")
   xy_triangle <- get_nodes_xy(dend, type = "triangle")
   expect_true(identical(xy_rectangle, xy_triangle))
   # par(mfrow = c(1,2))
   # plot(dend, type = "rectangle")
   # plot(dend, type = "triangle")
})


test_that("get_nodes_xy handles center parameter", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   xy_center_false <- get_nodes_xy(dend, center = FALSE)
   xy_center_true <- get_nodes_xy(dend, center = TRUE)
   expect_false(identical(xy_center_false, xy_center_true))
})

test_that("get_nodes_xy handles single node dendrogram", {
   dend <- as.dendrogram(hclust(dist(1:2)))
   xy <- get_nodes_xy(dend)
   expect_equal(dim(xy), c(3, 2))
})


