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
   expect_no_error(
      plot_horiz.dendrogram(dend, side = F)
   )
   
   # if horiz = F, should raise error
   expect_error(
      plot_horiz.dendrogram(dend, horiz = F)
   )
   # hclust object will be converted to dend, should work fine
   expect_no_error(
      plot_horiz.dendrogram(hc, side = T, edge.root = T, center = T)
   )
   
   # covers cases where text_pos is used but dleaf is not
   attr(dend, "edgetext") <- "t"
   capture.output(expect_no_error(
      plot_horiz.dendrogram(dend, side = T, edge.root = T, horiz = T, text_pos = 1)
   ))
   # covers cases where text_pos and dleaf used
   attr(dend, "edgetext") <- "t"
   capture.output(expect_no_error(
      plot_horiz.dendrogram(dend, side = T, edge.root = T, horiz = T, text_pos = 1, dLeaf = 1)
   ))
   # if a leaf is passed in
   expect_no_error(
      plot_horiz.dendrogram(dend[[1]], side = T, edge.root = T)
   )
   
   # create custom horiz class to bypass horiz = T requirement while still accessing lines that require horiz = F 
   horiz_trick <<- structure(FALSE, class = "horiz_trick")
   `!.horiz_trick` <<- function(x) FALSE
   capture.output(expect_no_error(
      plot_horiz.dendrogram(dend, side = T, edge.root = T, horiz = horiz_trick)
   ))
   
})


options(verbose = TRUE)
test_that("plotNode_horiz verbose works", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   attr(dend[[1]], "edgetext") <- "t"
   # general case which covers most lines
   capture.output(expect_no_error(
      plotNode_horiz(1, 1, dend, center = T, nodePar = NULL, leaflab = "perpendicular")
   ))
   
   # specific cases to cover all lines
   capture.output(expect_no_error(
      plotNode_horiz(0.5, 1, dend, center = T, nodePar = 1, horiz = F, leaflab = "perpendicular", edgePar = list(), type = "rectangle", dLeaf = NULL)
   ))
   capture.output(expect_no_error(
      plotNode_horiz(0.5, 1, dend, center = T, nodePar = 1, leaflab = "textlike", edgePar = list(), type = "rectangle", dLeaf = NULL)
   ))

   # with type triangle
   capture.output(expect_no_error(
      plotNode_horiz(0.5, 1, dend, center = T, nodePar = 1, leaflab = "textlike", horiz = T, edgePar = list(), type = "triangle", dLeaf = NULL)
   ))
   capture.output(expect_no_error(
      plotNode_horiz(0.5, 1, dend, center = T, nodePar = 1, horiz = F, leaflab = "perpendicular", edgePar = list(), type = "triangle", dLeaf = NULL)
   ))
   
   # if a leaf is passed in
   leaf <- dend[[1]]
   attr(leaf, "label") <- 1
   capture.output(expect_no_error(
      plotNode_horiz(0.5, 1, leaf, center = T, nodePar = 1, leaflab = "perpendicular", horiz = T, edgePar = list(), type = "triangle", dLeaf = NULL)
   ))
   # if ybot is null
   attr(dend[[1]], "height") <- NULL
   capture.output(expect_no_error(
      plotNode_horiz(0.5, 1, dend, center = T, nodePar = 1, leaflab = "textlike", horiz = T, edgePar = list(), type = "triangle", dLeaf = NULL)
   ))

})
options(verbose = FALSE)


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
   
   # temporarily overwrite par() function to access error that is otherwise inaccessible
   expect_error(with_mock(
      par = function(mar = 0) c(1,1),
      tanglegram.dendlist(dend12, just_one = F)
   ))
   # if only 1 dendrogram is passed in
   expect_error(
      tanglegram.dendlist(dend12[1])   
   )
   # if trying to plot trees that are not in the dendlist
   expect_error(
      tanglegram.dendlist(dend12, which = c(1,2,3))   
   )
   
   # if dendrograms with names are passed in
   names(dend12) <- c("dendrogram1", "dendrogram2")
   expect_no_error(
      tanglegram.dendlist(dend12)
   )
   
   # cover additional parameters
   expect_no_error(
      tanglegram.dendrogram(dend1, dend2, faster = T, remove_nodePar = T, sort = T, rank_branches = T, hang = T, 
                            lab.cex = 1, edge.lwd = 2, k_labels = 2, k_branches = 2)
   )
   expect_no_error(
      tanglegram.dendrogram(dend1, dend2, highlight_branches_col = T)
   )
   expect_no_error(
      tanglegram.dendrogram(dend1, dend2, dLeaf = 3)
   )
   
})
