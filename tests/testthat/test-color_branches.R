# library(testthat)
# library(dendextend)


test_that("color_branches works", {
   
   dend <- c(1:5) %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   
   # test case where k and colors are misaligned
   expect_warning(dend %>% color_branches(k = 3, col = c("red", "green")))
   expect_warning(dend %>% color_branches(k = 2, col = c("red", "green", "blue")))
   
   # test case where k and cluster are both provided
   expect_warning(dend %>% color_branches(k = 2, clusters = c(1, 1, 2, 2, 3)))
   
   # test case where number of cluster does not equal number of leaves
   expect_warning(dend %>% color_branches(clusters = c(1, 1, 2, 3)))
   
   # test case where labels are not unique, also tests when both k and h aren't specified
   labels(dend)[2] = 1
   expect_warning(dend %>% color_branches(warn = T))
   
   # test when non-dendrogram object is passed in
   x <- matrix(1:4, nrow = 2)
   suppressWarnings(expect_error(x %>% color_branches(k = 2)))
   hc <- c(1:5) %>%
      dist() %>%
      hclust()
   dend <- as.dendrogram(hc)
   expect_equal(
      labels(color_branches(hc)), 
      labels(color_branches(dend))
   ) # hc gets converted to dendrogram so should be the same
   
   # temporarily redefine cutree to return 0 so we can activate warning for 'dend has only one level'
   expect_warning(with_mock(
      `dendextend::cutree` = function(...) 0,
      color_branches(dend, k = 2, warn = T) 
   ))
   # trigger 'else' clause in addcol
   expect_no_error(color_branches(dend) %>% color_branches())
   
   # test when groupLabels argument used
   expect_error(
      color_branches(dend, k = 2, warn = T, groupLabels = function(...) TRUE)   
   )
   # structure doesnt change
   result1 <- color_branches(dend, k = 2, warn = T, groupLabels = TRUE)
   result2 <- color_branches(dend, k = 2, warn = T, groupLabels = FALSE) 
   expect_equal(
      labels(result1),
      labels(result2)
   )
   # should be labeled now
   suppressWarnings(expect_identical(
      dendrapply(result1, function(dend_node) attributes(dend_node))$members$edgetext$label, 
      "1"
   ))
   # should remain unlabeled
   suppressWarnings(expect_identical(
      dendrapply(result2, function(dend_node) attributes(dend_node))$members$edgetext$label, 
      NULL
   ))
})


test_that("color_labels works", {
   
   dend <- c(1:5) %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   
   # should recolor first and second label to red
   colorful_dend <- color_labels(dend, labels = c(1,2), col = "red")
   expect_identical(
      dendrapply(colorful_dend, function(dend_node) attributes(dend_node))$members$members$nodePar$lab.col,
      "red"
   )
   
   # color and labels vector have a length mismatch, creates warning because it has to recycle colors
   expect_warning(
      color_labels(dend, labels = c(3,4,5), col = c("red", "blue"), warn = T)
   )

   # if something other than a dend / hclust is passed in
   x <- matrix(1:4, nrow = 2)
   expect_error(color_labels(x))
   
   # if hclust is passed in
   hc <- c(1:5) %>%
      dist() %>%
      hclust()
   colorful_dend <- color_labels(hc, k = 2)
   expect_identical(
      dendrapply(colorful_dend, function(dend_node) attributes(dend_node))$members$members$nodePar$lab.col,
      "#CC476B"
   )
   
   # if misalignment in k and number of colors
   expect_warning(
      color_labels(dend, k = 3, col = c("red", "blue"), warn = T)
   )
   expect_warning(
      color_labels(dend, k = 2, col = c("red", "blue", "green"), warn = T)   
   )
   
})


test_that("lty_branches works", {
   
   dend <- c(1:5) %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   
   # dend branches for second cluster should be dashed (aka lty = 2)
   lty_dend = lty_branches(dend, k = 2)
   suppressWarnings(expect_identical(
      dendrapply(lty_dend, function(dend_node) attributes(dend_node))$class$class$edgePar$lty,
      2
   ))
})


test_that("leaf_Colors works", {
   
   dend <- c(1:5) %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   colorful_dend <- color_labels(dend, k = 2)
   
   # extract leaf labels (none for uncolored dendrogram vs. #CC476B and #009681 for colorful dendrogram)
   expect_true(
      all(is.na(leaf_Colors(dend, "label")))
   )
   expect_true(
      all(leaf_Colors(colorful_dend, "label") == c("#CC476B", "#CC476B", "#009681", "#009681", "#009681"))
   )
   
   # if something other than a dend 
   x <- matrix(1:4, nrow = 2)
   expect_error(leaf_Colors(x))
})


test_that("color_branches_by_clusters works", {
   
   dend <- c(1:5) %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   
   # cluster color for the first edge should be based on cluster # 2
   colorful_dend <- color_branches_by_clusters(dend, clusters = c(2,2,1,1,1), order_value = T)
   suppressWarnings(expect_identical(
      dendrapply(colorful_dend, function(dend_node) attributes(dend_node))$members$members$edgePar$col,
      "2"
   ))
   
   # if wrong number of clusters is passed in
   expect_error(
      color_branches_by_clusters(dend, clusters = c(2,2,1,1), order_value = T)
   )
   
})
