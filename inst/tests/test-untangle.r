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
