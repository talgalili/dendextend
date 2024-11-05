# library(testthat)
# library(dendextend)

context("Tanglegram")


test_that("plot_horiz.dendrogram works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)
   
   hc <- USArrests[1:10, ] %>%
     dist() %>%
     hclust() 
   dend <- as.dendrogram(hc)
   
   # covers cases where side = F
   options(verbose = TRUE)
   capture.output(expect_no_error(
      plot_horiz.dendrogram(dend, side = F)
   ))
   options(verbose = FALSE)
   
   
   # if horiz = F, should raise error
   expect_error(
      plot_horiz.dendrogram(dend, horiz = F)
   )
   # hclust object will be converted to dend, should work fine
   expect_no_error(
      plot_horiz.dendrogram(hc, side = T, edge.root = T, center = T)
   )
   
   # covers cases where text_pos and dleaf used
   attr(dend, "edgetext") <- "t"
   capture.output(expect_no_error(
      plot_horiz.dendrogram(dend, side = T, edge.root = T, horiz = T, text_pos = 1, dLeaf = 1)
   ))
   # if a leaf is passed in
   expect_no_error(
      plot_horiz.dendrogram(dend[[1]], side = T, edge.root = T)
   )
   
   # randomly simulate horiz to reach parts of the code that are otherwise impossible due to the requirement that horiz = T
   makeActiveBinding("horiz", function() sample(c(TRUE, FALSE), 1), .GlobalEnv)
   capture.output(expect_no_error(
      plot_horiz.dendrogram(dend, side = T, edge.root = T, horiz = horiz, text_pos = 1)
   ))
   rm("horiz", envir = .GlobalEnv)

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
      tanglegram.hclust(as.hclust(dend1), as.hclust(dend2))
   )
   expect_no_error(
      tanglegram.phylo(ape::as.phylo(dend1), ape::as.phylo(dend2))
   )
   expect_error(
      tanglegram.default(1:4, 4:1)
   )
})
