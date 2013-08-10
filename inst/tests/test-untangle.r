# require(testthat)


context("Untangle a dendrograms for plotting a tanglegram")


test_that("Match order of one dend based on another (using their labels)",{

   dend <- as.dendrogram(hclust(dist(USArrests[1:4,])))
   expect_identical(order.dendrogram(dend), c(4L, 3L, 1L, 2L))
   
   dend_changed <- dend
   order.dendrogram(dend_changed) <- 1:4
   expect_identical(order.dendrogram(dend_changed), c(1:4))

   # now let's fix the order of the new object to be as it was:
   dend_changed <- match_order_by_labels(dend_changed, dend)
   expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))
   
   # producing an error due to different labels in the two trees:
   # Fails when tree sizes are different:
   expect_error(match_order_by_labels(dend_changed[[-1]], dend)) 
   # It would also fail when one of the labels is different:
   labels(dend_changed)[1] <- "CAT"
   expect_error(match_order_by_labels(dend_changed, dend)) 
   
})



