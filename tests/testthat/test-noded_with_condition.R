# library(testthat)
# library(dendextend)


context("noded_with_condition")


test_that("noded_with_condition works", {
   dend <- iris[1:10, -5] %>%
     dist() %>%
     hclust() %>%
     as.dendrogram()
   
   high_enough <- function(sub_dend, height) attr(sub_dend, "height") > height
   expect_equal(
      noded_with_condition(dend, high_enough, height = 0.5, include_leaves = F, include_branches = T, na.rm = T),
      c(T, T, T, F, F, F, T, F, F)
   )
   expect_equal(
      noded_with_condition(dend, high_enough, height = 0.5, include_leaves = T, include_branches = F, na.rm = T),
      c(F, F, F, F, F, F, F, F, F, F)
   )
   
   # if condition parameter missing
   expect_error(
      noded_with_condition(dend)   
   )
   
})


test_that("which_leaf works", {
   # if non-dendrogram object passed in
   expect_error(
      which_leaf(1:4)
   )
})


test_that("which_node works", {
   dend <- iris[1:10, -5] %>%
     dist() %>%
     hclust() %>%
     as.dendrogram() %>%
     set("labels", 1:10)
   
   expect_equal(
      which_node(dend, c(1, 2)),
      2
   )
   
})