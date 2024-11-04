# library(testthat)
# library(dendextend)

context("Tanglegram")


test_that("plot_horiz.dendrogram works", {
   
   dend <- USArrests[1:10, ] %>%
     dist() %>%
     hclust() %>%
     as.dendrogram()
   
   expect_no_error(
      plot_horiz.dendrogram(dend, side = F)
   )
   
})


test_that("tanglegram works", {
   
   set.seed(23235)
   ss <- sample(1:150, 10)
   dend1 <- iris[ss, -5] %>%
     dist() %>%
     hclust("com") %>%
     as.dendrogram()
   dend2 <- iris[ss, -5] %>%
     dist() %>%
     hclust("sin") %>%
     as.dendrogram()
   dend12 <- dendlist(dend1, dend2)
   
   expect_no_error(
      tanglegram(dend12, common_subtrees_color_branches = TRUE)
   )
   
})