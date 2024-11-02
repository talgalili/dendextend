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
   expect_error(x %>% color_branches(clusters = c(1, 1)))
   hc <- c(1:5) %>%
      dist() %>%
      hclust()
   dend <- as.dendrogram(hc)
   expect_equal(color_branches(hc), color_branches(dend)) # hc gets converted to dendrogram so should be the same
   
   # temporarily redefine cutree to return 0 so we can activate warning for 'dend has only one level'
   expect_warning(with_mock(
      `dendextend::cutree` = function(...) 0,
      color_branches(dend, k = 2, warn = T) 
   ))
   
   # test when groupLabels argument used
   expect_error(
      color_branches(dend, k = 2, warn = T, groupLabels = function(...) TRUE)   
   )
   # structure doesnt change
   expect_equal(
      color_branches(dend, k = 2, warn = T, groupLabels = TRUE),
      color_branches(dend, k = 2, warn = T, groupLabels = FALSE) 
   )
   # should be labeled now
   expect_identical(
      expect_warning(dendrapply(result1, function(dend_node) attributes(dend_node))$members$edgetext$label), 
      "1"
   )
   # should remain unlabeled
   expect_identical(
      expect_warning(dendrapply(result2, function(dend_node) attributes(dend_node))$members$edgetext$label), 
      NULL
   )
})


