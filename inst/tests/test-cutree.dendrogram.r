# require(testthat)

context("Cutting a dendrogram")


test_that("Checking if a number is natural",{

   expect_true(is.natural.number(1))    # is TRUE
   expect_true(all(is.natural.number( seq(1,5, by=1)  )))
   expect_false(all(is.natural.number( seq(0,5, by=1)  )))
   expect_false(all(is.natural.number( seq(-1,5, by=0.5)  )))
   expect_error(is.natural.number( "a" ))
   
})



test_that("Rotate a dendrogram",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  
   
   #    labels(dend)
   #    rev(labels(dend))
   
   expect_equal(rev(labels(dend)), labels(rotate(dend, 3:1)))
   expect_equal(rev(labels(dend)), labels(rotate(dend, rev(labels(dend)))))
   
})
