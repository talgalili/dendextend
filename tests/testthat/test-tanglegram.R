# library(testthat)
# library(dendextend)

context("Tanglegram")


test_that("plot_horiz.dendrogram works", {
   
   dend <- USArrests[1:10, ] %>%
     dist() %>%
     hclust() %>%
     as.dendrogram()
   plot_horiz.dendrogram(dend, side = F)
})