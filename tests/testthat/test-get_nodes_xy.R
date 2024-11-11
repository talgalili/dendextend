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


options(verbose = TRUE)
test_that("plotNode2 works", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   reset_nodes_xy <- function(dend) {
      local({
         xy_matrix <- matrix(0, nrow = nnodes(dend), ncol = 2)
         i_node <- 0
         
         function(xy) {
            if (missing(xy)) {
               return(with(environment(nodes_xy), xy_matrix))
            }
            # Increment node and assign xy values
            i_node <<- i_node + 1
            xy_matrix[i_node, ] <<- xy
         }
      })
   }
   nodes_xy <- reset_nodes_xy(dend)
   attr(dend[[1]], "edgetext") <- "t"
   # general case which covers most lines
   plot.new()
   capture.output(expect_no_error(
      plotNode2(1, 1, dend, center = T, leaflab = "perpendicular", nodes_xy = nodes_xy)
   ))
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(1, 0.9, dend, center = F, type = "rectangle", leaflab = "perpendicular", nodes_xy = nodes_xy)
   ))
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(1, 0.9, dend, center = F, type = "rectangle", leaflab = "perpendicular", nodes_xy = nodes_xy)
   ))
   
   # specific cases to cover all lines
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(0.5, 1, dend, center = T, horiz = F, leaflab = "perpendicular", type = "rectangle", nodes_xy = nodes_xy)
   ))
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(0.5, 1, dend, center = T, leaflab = "textlike", type = "rectangle", nodes_xy = nodes_xy)
   ))
   
   # with type triangle
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(0.5, 1, dend, center = T, leaflab = "textlike", horiz = T, type = "triangle", nodes_xy = nodes_xy)
   ))
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(0.5, 1, dend, center = T, horiz = F, leaflab = "perpendicular", type = "triangle", nodes_xy = nodes_xy)
   ))
   
   # if a leaf is passed in
   leaf <- dend[[1]]
   attr(leaf, "label") <- 1
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(0.5, 1, leaf, center = T, leaflab = "perpendicular", horiz = T, type = "triangle", nodes_xy = nodes_xy)
   ))
   # if ybot is null
   attr(dend[[1]], "height") <- NULL
   nodes_xy <- reset_nodes_xy(dend)
   capture.output(expect_no_error(
      plotNode2(0.5, 1, dend, center = T, leaflab = "textlike", horiz = T, type = "triangle", nodes_xy = nodes_xy)
   ))
   
})
options(verbose = FALSE)