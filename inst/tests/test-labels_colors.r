context("Coloring tree's labels")


test_that("Coloring leaves works",{
   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   # Defaults:
   expect_equal(labels_colors(dend), NULL)
   
   # let's add some color:
   labels_colors(dend) <- 2:4
   expect_equal(labels_colors(dend), 2:4)
   
   # changing color to black, see that recycling works
   expect_warning(labels_colors(dend) <- 1)
   expect_equal(labels_colors(dend), rep(1,3))
   
   # removing color (and the nodePar completely - if it has no other attributed but lab.col)
   expect_warning(labels_colors(dend) <- NULL)   
   expect_equal(labels_colors(dend), NULL)
   
})

