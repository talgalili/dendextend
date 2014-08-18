context("Coloring tree's labels")


test_that("Leaves color is NULL",{
   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   # Defaults:
   expect_equal(labels_colors(dend), NULL)
})




test_that("Setting and extracting leaves color",{
   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   # let's add some color:
   labels_colors(dend) <- 2:4
   # getting it (without labels)
   expect_equal(labels_colors(dend, labels = FALSE), 2:4)
   # getting it (with labels)
   expect_equal(labels_colors(dend), structure(2:4, .Names = c("Arizona", "Alabama", "Alaska")))
   
})


test_that("Setting and extracting leaves color (some are NA)",{
   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   # let's add some color:
   labels_colors(dend) <- c(2, NA, 4)
   # getting it (without labels)
   expect_equal(labels_colors(dend, labels = FALSE), c(2, NA, 4))
   # getting it (with labels)
   expect_equal(labels_colors(dend), structure(c(2, NA, 4), .Names = c("Arizona", "Alabama", "Alaska")))
   
})



test_that("Color recycling",{
   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
      
   # changing color to black, see that recycling works
      dendextend_options("warn", TRUE)
   expect_warning(labels_colors(dend) <- 1)
      dendextend_options("warn", FALSE)
   expect_equal(labels_colors(dend, labels = FALSE), rep(1,3))

})


test_that("Color removing",{
   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   labels_colors(dend) <- 2:4
   
   # removing color (and the nodePar completely - if it has no other attributed but lab.col)
   expect_warning(labels_colors(dend) <- NULL)   
   expect_equal(labels_colors(dend), NULL)
   
})


test_that("Can't color a non-dendrogram object",{
   
   hc <- hclust(dist(USArrests[1:3,]), "ave")   
   
   expect_error(labels_colors(hc))   
   expect_error(labels_colors(hc) <- NULL)      
})


