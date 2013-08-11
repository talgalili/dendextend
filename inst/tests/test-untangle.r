# require(testthat)


context("Untangle two dendrograms for plotting a tanglegram")


test_that("shuffle works",{
   
   
   dend <- as.dendrogram(hclust(dist(USArrests)))
   set.seed(234238)
   dend2 <- shuffle(dend)
   
   # different tree layouts:
   #    tanglegram(dend, dend2, margin_inner=7)
   expect_identical(round(entanglement(dend, dend2), 3),  0.398)
   expect_false(identical(dend, dend2))
   
   # same topology
   expect_true(identical(sort(dend), sort(dend2)))
   
   
})



test_that("Flip leaves work",{
   dend1 <- as.dendrogram(hclust(dist(USArrests[1:5,])))
   dend2 <- flip_leaves(dend1, c(3,5), c(1,2))
   # tanglegram(dend1,dend2)

   expect_identical(entanglement(dend1,dend2, L = 2),  0.4)   
})



