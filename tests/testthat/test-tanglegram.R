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
   suppressWarnings(RNGversion("3.5.0"))
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
   
   # if non-dendrogram objects are passed in
   expect_no_error(
      tanglegram(as.hclust(dend1), as.hclust(dend2))
   )
   expect_no_error(
      tanglegram(ape::as.phylo(dend1), ape::as.phylo(dend2))
   )
   expect_error(
      tanglegram(1:4, 4:1)
   )
   
   
})
