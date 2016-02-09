# library(testthat)

context("Tree size")


test_that("Get a hclust number of leaves",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   
   expect_true(nleaves(hc)==3L)
})


test_that("Get a dendrogram number of leaves",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  
   
   expect_true(nleaves(dend)==3L)
})


test_that("nleaves works the same for members and order",{
   dend <- as.dendrogram(hclust(dist(USArrests)))
   expect_identical(nleaves(dend, "order"), nleaves(dend, "members"))
})



test_that("Get a dendrogram number of nodes",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  
   
   expect_true(nnodes(dend)==5L)
   expect_true(nnodes(hc)==5L)
})


test_that("Can't get a tree size for an object which is not hclust, dendrogram or phylo",{
   x <- 1:4
   expect_error(nleaves(x))
   x <- letters[1:4]
   expect_error(nleaves(x))
   x <- list(1:4)
   expect_error(nleaves(x))
})



test_that("Get a dendrogram tree size with leaves having their branch of height 0",{

   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   ###
   # Trivial case
#    count_terminal_nodes(dend) # 3 terminal nodes
#    nleaves(dend) # 3 - the same number
   expect_true(count_terminal_nodes(dend)==nleaves(dend))
   
   ###
   # NON-Trivial case
#    str(dend)
   attr(dend[[2]], "height") <- 0
#    count_terminal_nodes(dend) # 2 terminal nodes, why? see this plot:
#    plot(dend, main = "This is considered a tree \n with TWO terminal nodes only") # while we have 3 leaves, in practice we have only 2 terminal nodes (this is a feature, not a bug.)
   expect_false(count_terminal_nodes(dend)==nleaves(dend))
})


