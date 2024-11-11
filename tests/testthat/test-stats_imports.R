# library(testthat)
# library(dendextend)


context("stats_imports")


test_that("stats_.memberDend works", {
   x <- matrix(1:4)
   expect_identical(
      stats_.memberDend(x),
      1L
   )
})


test_that("stats_midcache.dendrogram works", {
   
   # create custom class to reach specific error
   dend <- as.dendrogram(hclust(dist(mtcars)))
   class(dend) <- c("custom_dend", class(dend))
   length.custom_dend <<- function(x) {
      -1
   }
   
   expect_error(
      stats_midcache.dendrogram(dend)   
   )
   
})


options(verbose = TRUE)
test_that("stats_plotNode works", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   attr(dend[[1]], "edgetext") <- "t"
   # general case which covers most lines
   plot.new()
   capture.output(expect_no_error(
      stats_plotNode(1, 1, dend, center = T, nodePar = NULL, leaflab = "perpendicular")
   ))
   capture.output(expect_no_error(
      stats_plotNode(1, 0.9, dend, center = F, edgePar = list(), type = "rectangle", nodePar = list("pch" = 1:2), leaflab = "perpendicular", dLeaf = NULL)
   ))
   capture.output(expect_no_error(
      stats_plotNode(1, 0.9, dend, center = F, edgePar = list(), type = "rectangle", nodePar = NULL, leaflab = "perpendicular", dLeaf = NULL)
   ))
   
   # specific cases to cover all lines
   capture.output(expect_no_error(
      stats_plotNode(0.5, 1, dend, center = T, nodePar = 1, horiz = F, leaflab = "perpendicular", edgePar = list(), type = "rectangle", dLeaf = NULL)
   ))
   capture.output(expect_no_error(
      stats_plotNode(0.5, 1, dend, center = T, nodePar = 1, leaflab = "textlike", edgePar = list(), type = "rectangle", dLeaf = NULL)
   ))
   
   # with type triangle
   capture.output(expect_no_error(
      stats_plotNode(0.5, 1, dend, center = T, nodePar = 1, leaflab = "textlike", horiz = T, edgePar = list(), type = "triangle", dLeaf = NULL)
   ))
   capture.output(expect_no_error(
      stats_plotNode(0.5, 1, dend, center = T, nodePar = 1, horiz = F, leaflab = "perpendicular", edgePar = list(), type = "triangle", dLeaf = NULL)
   ))
   
   # if a leaf is passed in
   leaf <- dend[[1]]
   attr(leaf, "label") <- 1
   capture.output(expect_no_error(
      stats_plotNode(0.5, 1, leaf, center = T, nodePar = 1, leaflab = "perpendicular", horiz = T, edgePar = list(), type = "triangle", dLeaf = NULL)
   ))
   # if ybot is null
   attr(dend[[1]], "height") <- NULL
   capture.output(expect_no_error(
      stats_plotNode(0.5, 1, dend, center = T, nodePar = 1, leaflab = "textlike", horiz = T, edgePar = list(), type = "triangle", dLeaf = NULL)
   ))
   
})
options(verbose = FALSE)



