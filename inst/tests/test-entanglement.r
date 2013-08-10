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




test_that("Match order of one dend based on another (using their order valuess)",{
   
   dend <- as.dendrogram(hclust(dist(USArrests[1:4,])))
   expect_identical(order.dendrogram(dend), c(4L, 3L, 1L, 2L))
   
   dend_changed <- dend
   order.dendrogram(dend_changed) <- 1:4
   expect_identical(order.dendrogram(dend_changed), c(1:4))
   
   # now let's fix the order of the new object to be as it was:
   dend_changed <- match_order_dendrogram_by_old_order(dend_changed, dend, 
                                                        order.dendrogram(dend_changed))
   expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))

   
   # Watch this!
   dend_changed <- dend
   dend_changed <- rev(dend_changed)
   expect_false(identical(order.dendrogram(dend_changed), order.dendrogram(dend)))
   # we keep the order of dend_change, so that the leaves order are synced
   # with their labels JUST LIKE dend:
   old_dend_changed_order <- order.dendrogram(dend_changed)   
   # now we change dend_changed leaves order values:
   order.dendrogram(dend_changed) <- 1:4
   # and we can fix them again, based on their old kept leaves order:
   dend_changed <- match_order_dendrogram_by_old_order(dend_changed, dend, 
                                                        old_dend_changed_order)
   expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))
   
})



